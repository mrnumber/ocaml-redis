module Test_lwt = Test.Make(Redis_lwt.IO)

let _ = Test_lwt.test ()
