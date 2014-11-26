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

let redis_string_bucket() =
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
    | false -> assert_failure("Can't connect to Redis server")

(* ECHO *)
let test_case_echo conn =
  match Redis_sync.Client.echo conn "ECHO" with
    | Some "ECHO" -> ()
    | _ -> assert_failure("Can't echo to Redis server")

(* Keys test case *)
let test_case_keys conn =
  let module R = Redis_sync.Client in
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  assert_bool "Can't set key" (R.set conn key value = ()) ;
  assert_bool "Key and value mismatch" (R.get conn key = Some value);
  assert_bool "Key doesn't exist" (R.exists conn key = true);
  assert_bool "Can't find with itself as a pattern in KEYS command" (R.keys conn key = [key]);
  assert_bool "Can't find key with RANDOMKEY command" (R.randomkey conn = Some key);
  assert_bool "Can't move key to redis database #2" (R.move conn key 2 = true);
  assert_bool "Can't select redis database #2" (R.select conn 2 = ());
  assert_bool "Can't set expiration timeout for key" (R.expire conn key 1 = true);
  assert_bool "Can't set expiration timeout in milliseconds for key" (R.pexpire conn key 1000 = true);
  assert_bool "Can't check expiration timeout for key" (List.mem (R.ttl conn key) [Some 0; Some 1]);
  (match (R.pttl conn key) with
   | Some pttl -> assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000);
   | None -> assert_failure "Can't check expiration timeout for key");
  assert_bool "Key wasn't deleted" (R.del conn [key] = 1)

let bracket test_case () =
  let conn = setup () in
  let _ = test_case conn in
  teardown conn

let _ =
  let suite = "Redis" >::: [
    "test_case_ping" >:: (bracket test_case_ping);
    "test_case_echo" >:: (bracket test_case_echo);
    "test_case_keys" >:: (bracket test_case_keys);
  ] in
  Random.self_init () ;
  run_test_tt_main suite
