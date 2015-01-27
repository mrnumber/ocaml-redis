open OUnit
open Redis
open Sys

let redis_test_host () =
  try
    Sys.getenv("OCAML_REDIS_TEST_IP")
  with Not_found ->
    "127.0.0.1"

let redis_test_port () =
  try
    int_of_string(Sys.getenv("OCAML_REDIS_TEST_PORT"))
  with Not_found ->
    6379

let redis_string_bucket () =
  let number = Random.bits () in
  "ounit_" ^ string_of_int(number)

let redis_integer_bucket = Random.bits

let redis_float_bucket () =
  (* Redis' float operations precision differs from OCaml's float operation
     precision.  Limit our floats to 11 digits after the decimal point to have
     possibility to test float operations. *)
  let a = float_of_int (Random.bits ()) in
  let b = float_of_int (Random.bits ()) in
  float_of_string (Printf.sprintf "%.8f" (a /. b))

let redis_n_strings_bucket n =
  let rec helper acc n =
    if n = 0 then acc else helper (redis_string_bucket () :: acc) (n - 1) in
  helper [] n


module Make(IO : Redis.Make.IO) = struct

  module Client = Redis.Client.Make(IO)

  let (>>=) = IO.(>>=)
  let (>>|) x f = x >>= fun x -> IO.return (f x)

  let redis_open_connection =
    let host = redis_test_host () in
    let port = redis_test_port () in
    Client.(connect {host; port})

  let setup _ = redis_open_connection

  let teardown _ = IO.return ()

  let io_assert msg check result =
    IO.return (assert_bool msg (check result))

  (* PING *)
  let test_case_ping conn =
    Client.ping conn >>=
    io_assert "Can't connect to Redis server" ((=) true)

  (* ECHO *)
  let test_case_echo conn =
    Client.echo conn "ECHO" >>=
    io_assert "Can't echo to Redis server" ((=) (Some "ECHO"))

  (* INFO *)
  let test_case_info conn =
    Client.info conn >>| fun result ->
    (let tcp_port = List.filter (fun (k, v) -> k = "tcp_port") result
                    |> List.hd |> snd in
     assert_bool "Got wrong data about port with INFO command"
       (int_of_string tcp_port = redis_test_port()))

  (* Keys test case *)
  let test_case_keys conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    let key' = redis_string_bucket () in
    let key'' = redis_string_bucket () in

    Client.set conn key value >>=
    io_assert "Can't set key" ((=) ()) >>= fun () ->
    Client.get conn key >>=
    io_assert "Key and value mismatch" ((=) (Some value)) >>= fun () ->
    Client.exists conn key >>=
    io_assert "Key doesn't exist" ((=) true) >>= fun () ->
    Client.keys conn key >>=
    io_assert "Can't find with itself as a pattern in KEYS command"
      (fun keys -> let found_key = List.find (fun k -> k = key) keys in
        (found_key = key)) >>= fun () ->
    Client.randomkey conn >>=
    io_assert "Can't find key with RANDOMKEY command" ((<>) None) >>= fun () ->
    Client.move conn key 2 >>=
    io_assert "Can't move key to redis database #2" ((=) true) >>= fun () ->
    Client.select conn 2 >>=
    io_assert "Can't select redis database #2" ((=) ()) >>= fun () ->
    Client.rename conn key key' >>=
    io_assert "Can't rename key" ((=) ()) >>= fun () ->
    Client.set conn key'' value >>=
    io_assert "Can't set key''" ((=) ()) >>= fun () ->
    Client.renamenx conn key' key'' >>=
    io_assert "Can renamenx key" ((=) false) >>= fun () ->
    Client.rename conn key' key >>=
    io_assert "Can't rename key" ((=) ()) >>= fun () ->
    Client.del conn [key; key''] >>=
    io_assert "Key wasn't deleted" ((=) 2) >>= fun () ->
    Client.select conn 0 >>=
    io_assert "Can't select redis database #0" ((=) ())

  let test_case_multiple_keys conn =
    let keys = redis_n_strings_bucket 10 in
    let values = List.rev keys in
    let kv_pairs = List.combine keys values in
    Client.mset conn kv_pairs >>=
    io_assert "Can't set multiple keys" ((=) ()) >>= fun () ->

    let expected_values = List.map (fun x -> Some x) values in
    Client.mget conn keys >>| (fun actual_values ->
      (List.iter2
         (fun expected actual -> assert_bool "Got unexpected value" (expected = actual))
         expected_values actual_values)) >>= fun () ->

    let another_values = redis_n_strings_bucket 10 in
    let kv_pairs = List.combine keys another_values in
    Client.msetnx conn kv_pairs >>=
    io_assert "It's possible MSETNX multiple keys" ((=) false) >>= fun () ->

    let another_keys = redis_n_strings_bucket 10 in
    let kv_pairs = List.combine another_keys another_values in
    Client.msetnx conn kv_pairs >>=
    io_assert "Can't MSETNX multiple keys" ((=) true)

  let test_case_dump_restore conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) ()) >>= fun () ->
    Client.dump conn key >>= function
    | None -> assert_failure "Can't dump value"
    | Some value_dump ->
      let key' = String.concat "" [key; redis_string_bucket ()] in
      Client.restore conn key' 0 value_dump >>=
      io_assert "Can't restore value" ((=) ()) >>= fun () ->
      Client.get conn key' >>=
      io_assert "Key value and restored value mismatch" ((=) (Some value))

  let test_case_expire conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) ()) >>= fun () ->
    Client.expire conn key 1 >>=
    io_assert "Can't set expiration timeout for key" ((=) true) >>= fun () ->
    Client.pexpire conn key 1000 >>=
    io_assert "Can't set expiration timeout in milliseconds for key" ((=) true) >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key"
      (fun x -> List.mem x [Some 0; Some 1]) >>= fun () ->
    Client.pttl conn key >>| (function
      | Some pttl -> assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000)
      | None -> assert_failure "Can't check expiration timeout for key")
    >>= fun () ->
    Client.persist conn key >>=
    io_assert "Can't remove existing timeout on key" ((=) true)  >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key" ((=) None)

  let test_case_expireat conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) ()) >>= fun () ->
    let expiry = Unix.time () +. 1. in
    Client.expireat conn key expiry >>=
    io_assert "Can't set expiration timeout for key" ((=) true) >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key"
      (fun x -> List.mem x [Some 0; Some 1]) >>= fun () ->

    let pexpiry = int_of_float (Unix.time ()) * 1000 + 1000 in
    Client.pexpireat conn key pexpiry >>=
    io_assert "Can't set expiration timeout for key (in ms)" ((=) true) >>= fun () ->
    Client.pttl conn key >>| function
    | Some pttl ->
      assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000)
    | None ->
      assert_failure "Can't check expiration timeout for key"

  let test_case_type conn =
    let value = redis_string_bucket () in
    let string_key = redis_string_bucket () in
    Client.set conn string_key value >>=
    io_assert "Can't set key" ((=) ()) >>= fun () ->
    let list_key = redis_string_bucket () in
    Client.lpush conn list_key value >>=
    io_assert "Can't push value to list" ((=) 1) >>= fun () ->

    Client.type_of conn string_key >>=
    io_assert "Got wrong key type for string_key" ((=) `String) >>= fun () ->
    Client.type_of conn list_key >>=
    io_assert "Got wrong key type for list_key" ((=) `List)

  (* APPEND *)
  let test_case_append conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.append conn key value >>=
    io_assert "Can't append initial value to key"
      (fun x -> x = String.length value) >>= fun () ->
    Client.append conn key value >>=
    io_assert "Can't append additional value to key"
      (fun x -> x = (String.length value + String.length value)) >>= fun () ->
    Client.get conn key >>=
    io_assert "Can't get key" ((=) (Some (String.concat "" [value; value])))

  (* INCR/DECR/INCRBY/DECRBY/INCRBYFLOAT *)
  let test_case_incr_decr conn =
    let key = redis_string_bucket () in
    let value = redis_integer_bucket () in
    let increment = redis_integer_bucket () in
    Client.set conn key (string_of_int value) >>=
    io_assert "Can't set float value to key" ((=) ()) >>= fun () ->
    Client.incrby conn key increment >>=
    io_assert "Can't increment value by integer" ((=) (value + increment)) >>= fun () ->
    Client.incr conn key >>=
    io_assert "Can't increment value by one" ((=) (value + increment + 1)) >>= fun () ->
    Client.decrby conn key increment >>=
    io_assert "Can't decrement value by integer" ((=) (value + 1)) >>= fun () ->
    Client.decr conn key >>=
    io_assert "Can't decrement value by one" ((=) value) >>= fun () ->
    Client.incrbyfloat conn key 2. >>=
    io_assert "Can't increment value by float"
      ((=) (float_of_int value +. 2.)) >>= fun () ->
    Client.incrbyfloat conn key (- 2.) >>=
    io_assert "Can't increment value by negative float"
      ((=) (float_of_int value))

  (* BITOP/BITCOUNT/BITPOS/GETBIT/SETBIT *)
  let test_case_bit_operations conn =
    let dest = redis_string_bucket () in
    let key1 = redis_string_bucket () in
    let key2 = redis_string_bucket () in
    let value1 = "foobar" in
    let value2 = "abcdef" in
    let value3 = "\x00\xff\xf0" in
    Client.set conn key1 value1 >>=
    io_assert "Can't set value1 to key1" ((=) ()) >>= fun () ->
    Client.set conn key2 value2 >>=
    io_assert "Can't set value2 to key2" ((=) ()) >>= fun () ->
    Client.bitop conn Client.AND dest [key1; key2] >>=
    io_assert "Can't execute BITOP AND key1 and key2" ((=) 6) >>= fun () ->
    Client.get conn dest >>=
    io_assert "Got unexpected value from dest" ((=) (Some "`bc`ab")) >>= fun () ->
    Client.bitop conn Client.NOT dest [key1] >>=
    io_assert "Can't execute BITOP NOT key1" ((=) 6) >>= fun () ->
    Client.get conn dest >>=
    io_assert "Got unexpected value from dest" ((=) (Some "\x99\x90\x90\x9d\x9e\x8d")) >>= fun () ->
    Client.set conn key1 value3 >>=
    io_assert "Can't set value3 to key1" ((=) ()) >>= fun () ->
    Client.bitpos conn key1 1 >>=
    io_assert "Got unexpected bit position" ((=) 8) >>= fun () ->
    Client.bitpos conn key1 1 ~first:0 >>=
    io_assert "Got unexpected bit position" ((=) 8) >>= fun () ->
    Client.bitpos conn key1 1 ~first:2 >>=
    io_assert "Got unexpected bit position" ((=) 16) >>= fun () ->
    Client.getbit conn key1 0 >>=
    io_assert "Can't get bit" ((=) 0) >>= fun () ->
    Client.setbit conn key1 0 1 >>=
    io_assert "Can't set bit" ((=) 0) >>= fun () ->
    Client.getbit conn key1 0 >>=
    io_assert "Can't get bit" ((=) 1) >>= fun () ->
    Client.set conn key1 value1 >>=
    io_assert "Can't set value1 to key1" ((=) ()) >>= fun () ->
    Client.bitcount conn key1 >>=
    io_assert "Got unexpected bit count" ((=) 26) >>= fun () ->
    Client.bitcount conn key1 ~first:1 >>=
    io_assert "Got unexpected bit count" ((=) 22) >>= fun () ->
    Client.bitcount conn key1 ~first:0 ~last:0 >>=
    io_assert "Got unexpected bit count" ((=) 4) >>= fun () ->
    Client.bitcount conn key1 ~first:1 ~last:1 >>=
    io_assert "Got unexpected bit count" ((=) 6)

  let test_case_scan conn =
    let rec scan_keys cursor keys =
      Client.scan conn cursor >>= fun (next_cursor, next_keys) ->
      let next_keys = List.concat [keys; next_keys] in
      if next_cursor == 0 then
        IO.return next_keys
      else
        scan_keys next_cursor next_keys in
    let scan_all_keys () = scan_keys 0 [] in
    Client.keys conn "*" >>= fun keys ->
    scan_all_keys () >>=
    io_assert "Number of keys got with KEYS command is not equal to number of keys got with SCAN command"
      (fun scanned_keys -> List.length keys = List.length scanned_keys)

  let bracket test_case () =
    IO.run
      (setup () >>= fun conn ->
       test_case conn >>= fun () ->
       teardown conn)

  let test () =
    let suite = "Redis" >::: [
      "test_case_ping" >:: (bracket test_case_ping);
      "test_case_echo" >:: (bracket test_case_echo);
      "test_case_info" >:: (bracket test_case_info);
      "test_case_keys" >:: (bracket test_case_keys);
      "test_case_multiple_keys" >:: (bracket test_case_multiple_keys);
      "test_case_dump_restore" >:: (bracket test_case_dump_restore);
      "test_case_expire" >:: (bracket test_case_expire);
      "test_case_expireat" >:: (bracket test_case_expireat);
      "test_case_type" >:: (bracket test_case_type);
      "test_case_append" >:: (bracket test_case_append);
      "test_case_incr_decr" >:: (bracket test_case_incr_decr);
      "test_case_bit_operations" >:: (bracket test_case_bit_operations);
      "test_case_scan" >:: (bracket test_case_scan);
    ] in
    Random.self_init ();
    run_test_tt_main suite
end
