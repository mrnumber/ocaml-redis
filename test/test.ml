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
  let bucket = ("ounit_" ^ string_of_int(number)) in
  bucket

let redis_integer_bucket () =
  let number = Random.bits () in
  number

let redis_float_bucket () =
  (* Redis' float operations precision differs from OCaml's float operation precision.
     Limit our floats to 11 digits after the decimal point to
     have possibility to test float operations. *)
  let a = float_of_int (Random.bits ()) in
  let b = float_of_int (Random.bits ()) in
  float_of_string (Printf.sprintf "%.8f" (a /. b))

let redis_open_connection =
  let host = redis_test_host () in
  let port = redis_test_port () in
  Redis_sync.Client.connect {host=host; port=port}

let setup _ =
  redis_open_connection

let teardown conn =
  ()

(* PING *)
let test_case_ping conn =
  match Redis_sync.Client.ping conn with
    | true -> ()
    | false -> assert_failure "Can't connect to Redis server"

(* ECHO *)
let test_case_echo conn =
  match Redis_sync.Client.echo conn "ECHO" with
    | Some "ECHO" -> ()
    | _ -> assert_failure "Can't echo to Redis server"

(* Keys test case *)
let test_case_keys conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  assert_bool "Can't set key" (R.set conn key value = ());
  assert_bool "Key and value mismatch" (R.get conn key = Some value);
  assert_bool "Key doesn't exist" (R.exists conn key);
  assert_bool "Can't find with itself as a pattern in KEYS command"
              (List.find (fun k -> k = key) (R.keys conn key) = key);
  assert_bool "Can't find key with RANDOMKEY command" (R.randomkey conn <> None);
  assert_bool "Can't move key to redis database #2" (R.move conn key 2);
  assert_bool "Can't select redis database #2" (R.select conn 2 = ());
  let key' = redis_string_bucket() in
  assert_bool "Can't rename key" (R.rename conn key key' = ());
  let key'' = redis_string_bucket() in
  assert_bool "Can't set key''" (R.set conn key'' value = ());
  assert_bool "Can renamenx key" (R.renamenx conn key' key'' = false);
  assert_bool "Can't rename key" (R.rename conn key' key = ());
  assert_bool "Key wasn't deleted" (R.del conn [key; key''] = 2);
  assert_bool "Can't select redis database #0" (R.select conn 0 = ())

let test_case_dump_restore conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  assert_bool "Can't set key" (R.set conn key value = ());
  match (R.dump conn key) with
  | None -> assert_failure "Can't dump value"
  | Some value_dump ->
     let key' = String.concat "" [key; redis_string_bucket()] in
     assert_bool "Can't restore value" (R.restore conn key' 0 value_dump = ());
     assert_bool "Key and restored value mismatch" (R.get conn key' = Some value)

let test_case_expire conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  assert_bool "Can't set key" (R.set conn key value = ());
  assert_bool "Can't set expiration timeout for key" (R.expire conn key 1);
  assert_bool "Can't set expiration timeout in milliseconds for key" (R.pexpire conn key 1000);
  assert_bool "Can't check expiration timeout for key" (List.mem (R.ttl conn key) [Some 0; Some 1]);
  (match (R.pttl conn key) with
   | Some pttl -> assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000);
   | None -> assert_failure "Can't check expiration timeout for key");
  assert_bool "Can't remove existing timeout on key" (R.persist conn key);
  assert_bool "Can't check expiration timeout for key" (R.ttl conn key = None)

let test_case_expireat conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  assert_bool "Can't set key" (R.set conn key value = ());
  let expiry = Unix.time () +. 1. in
  assert_bool "Can't set expiration timeout for key" (R.expireat conn key expiry);
  assert_bool "Can't check expiration timeout for key" (List.mem (R.ttl conn key) [Some 0; Some 1]);

  let pexpiry = int_of_float (Unix.time ()) * 1000 + 1000 in
  assert_bool "Can't set expiration timeout for key (in ms)" (R.pexpireat conn key pexpiry);
  match (R.pttl conn key) with
  | Some pttl -> assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000)
  | None -> assert_failure "Can't check expiration timeout for key"

let test_case_type conn =
  let module R = Redis_sync.Client in
  let value = redis_string_bucket() in

  let string_key = redis_string_bucket() in
  assert_bool "Can't set key" (R.set conn string_key value = ());

  let list_key = redis_string_bucket() in
  assert_bool "Can't push value to list" (R.lpush conn list_key value = 1);

  assert_bool "Got wrong key type for string_key" (R.type_of conn string_key = `String);
  assert_bool "Got wrong key type for list_key" (R.type_of conn list_key = `List)

(* APPEND *)
let test_case_append conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  assert_bool "Can't append initial value to key" (R.append conn key value = String.length value);
  assert_bool "Can't append additional value to key"
              (R.append conn key value = (String.length value + String.length value));
  assert_bool "Can't get key" (R.get conn key = Some (String.concat "" [value; value]))

(* INCR/DECR/INCRBY/DECRBY/INCRBYFLOAT *)
let test_case_incr_decr conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_integer_bucket () in
  let increment = redis_integer_bucket () in
  assert_bool "Can't set float value to key" (R.set conn key (string_of_int value) = ());
  assert_bool "Can't increment value by integer" (R.incrby conn key increment = value + increment);
  assert_bool "Can't increment value by one" (R.incr conn key = value + increment + 1);
  assert_bool "Can't decrement value by integer" (R.decrby conn key increment = value + 1);
  assert_bool "Can't decrement value by one" (R.decr conn key = value);
  assert_bool "Can't increment value by float"
              (R.incrbyfloat conn key 2. = (float_of_int value) +. 2.);
  assert_bool "Can't increment value by negative float"
              (R.incrbyfloat conn key (- 2.) = (float_of_int value))

(* BITOP/BITCOUNT/BITPOS/GETBIT/SETBIT *)
let test_case_bit_operations conn =
  let module R = Redis_sync.Client in
  let dest = redis_string_bucket() in
  let key1 = redis_string_bucket() in
  let key2 = redis_string_bucket() in
  let value1 = "foobar" in
  let value2 = "abcdef" in
  let value3 = "\x00\xff\xf0" in
  assert_bool "Can't set value1 to key1" (R.set conn key1 value1 = ());
  assert_bool "Can't set value2 to key2" (R.set conn key2 value2 = ());
  assert_bool "Can't execute BITOP AND key1 and key2" (R.bitop conn R.AND dest [key1; key2] = 6);
  assert_bool "Got unexpected value from dest" (R.get conn dest = Some "`bc`ab");
  assert_bool "Can't execute BITOP NOT key1" (R.bitop conn R.NOT dest [key1] = 6);
  assert_bool "Got unexpected value from dest" (R.get conn dest = Some "\x99\x90\x90\x9d\x9e\x8d");
  assert_bool "Can't set value3 to key1" (R.set conn key1 value3 = ());
  assert_bool "Got unexpected bit position" (R.bitpos conn key1 1 = 8);
  assert_bool "Got unexpected bit position" (R.bitpos conn key1 1 ~first:0 = 8);
  assert_bool "Got unexpected bit position" (R.bitpos conn key1 1 ~first:2 = 16);
  assert_bool "Can't get bit" (R.getbit conn key1 0 = 0);
  assert_bool "Can't set bit" (R.setbit conn key1 0 1 = 0);
  assert_bool "Can't get bit" (R.getbit conn key1 0 = 1);
  assert_bool "Can't set value1 to key1" (R.set conn key1 value1 = ());
  assert_bool "Got unexpected bit count" (R.bitcount conn key1 = 26);
  assert_bool "Got unexpected bit count" (R.bitcount conn key1 ~first:1 = 22);
  assert_bool "Got unexpected bit count" (R.bitcount conn key1 ~first:0 ~last:0 = 4);
  assert_bool "Got unexpected bit count" (R.bitcount conn key1 ~first:1 ~last:1 = 6)

let test_case_scan conn =
  let module R = Redis_sync.Client in
  let rec scan_keys cursor keys =
    let next_cursor, next_keys = R.scan conn cursor in
    let next_keys = List.concat [keys; next_keys] in
    if next_cursor == 0 then
      next_keys
    else
      scan_keys next_cursor next_keys in
  let scan_all_keys () = scan_keys 0 [] in
  assert_bool "Number of keys got with KEYS command is not equal to number of keys got with SCAN command"
              (List.length (R.keys conn "*") = List.length (scan_all_keys ()))

let bracket test_case () =
  let conn = setup () in
  let _ = test_case conn in
  teardown conn

let _ =
  let suite = "Redis" >::: [
    "test_case_ping" >:: (bracket test_case_ping);
    "test_case_echo" >:: (bracket test_case_echo);
    "test_case_keys" >:: (bracket test_case_keys);
    "test_case_dump_restore" >:: (bracket test_case_dump_restore);
    "test_case_expire" >:: (bracket test_case_expire);
    "test_case_type" >:: (bracket test_case_type);
    "test_case_append" >:: (bracket test_case_append);
    "test_case_incr_decr" >:: (bracket test_case_incr_decr);
    "test_case_bit_operations" >:: (bracket test_case_bit_operations);
    "test_case_scan" >:: (bracket test_case_scan);
  ] in
  Random.self_init ();
  run_test_tt_main suite
