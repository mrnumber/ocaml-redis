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
  assert_bool "Can't find with itself as a pattern in KEYS command" (R.keys conn key = [key]);
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
  ] in
  Random.self_init ();
  run_test_tt_main suite
