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

let redis_open_connection =
  let host = redis_test_host () in
  let port = redis_test_port () in
  Redis_sync.Client.connect {host=host; port=port}

let setup _ =
  redis_open_connection

let teardown conn =
  ()

let test_case_ping conn =
  match Redis_sync.Client.ping conn with
    | true -> ()
    | false -> assert_failure("Can't connect to Redis server")

let test_case_echo conn =
  match Redis_sync.Client.echo conn "ECHO" with
    | Some "ECHO" -> ()
    | _ -> assert_failure("Can't echo to Redis server")

let bracket test_case () =
  let conn = setup () in
  let _ = test_case conn in
  teardown conn

let _ =
  let suite = "Redis" >::: [
    "test_case_ping" >:: (bracket test_case_ping);
    "test_case_echo" >:: (bracket test_case_echo);
  ] in
  run_test_tt_main suite
