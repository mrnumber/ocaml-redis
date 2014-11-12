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

(* SET *)
(* GET *)
let test_case_set_string conn =
  let key = redis_string_bucket() in
  let value = redis_string_bucket() in
  let err = Redis_sync.Client.set conn key value in
  let result = Redis_sync.Client.get conn key in
  assert_bool "Can't set key" (err = ()) ;
  assert_bool "Key and value mismatch" (result = Some value)

let bracket test_case () =
  let conn = setup () in
  let _ = test_case conn in
  teardown conn

let _ =
  let suite = "Redis" >::: [
    "test_case_ping" >:: (bracket test_case_ping);
    "test_case_echo" >:: (bracket test_case_echo);
    "test_case_set_string" >:: (bracket test_case_set_string);
  ] in
  Random.self_init () ;
  run_test_tt_main suite
