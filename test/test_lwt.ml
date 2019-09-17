module Test_lwt = Test.Make(Redis_lwt.Client)
module Test_lwt_cluster = Test.Make(Redis_lwt.ClusterClient)

open OUnit

let suite =
  "lwt" >::: [
    Test_lwt.suite "simple";
    Test_lwt_cluster.suite "cluster";
  ]

let () =
  Random.self_init ();
  let res = run_test_tt suite in
  Test_lwt.teardown ();
  exit @@ Test.test_exit_code res
