(* error response *)
exception Error of string

module Make(IO:Make.IO) = struct
  module Client = Client.Make(IO)

  let (>>=) = IO.(>>=)
  let (>>) x y = x >>= fun _ -> y

  let acquire conn ?(atime=10.) ?(ltime=10) mutex id =
    let ttl = Unix.time() +. atime in

    let update_ttl () =
      Client.ttl conn mutex >>= function
        | None -> Client.expire conn mutex ltime >> IO.return ()
        | _ -> IO.return () in

    let rec loop () =
      Client.setnx conn mutex id >>= function
        | true -> Client.expire conn mutex ltime >> IO.return ()
        | _ -> update_ttl () >>
            if Unix.time() < ttl then IO.sleep(0.1) >> loop ()
            else IO.fail (Error "could not acquire lock")
    in
    loop ()

  let release conn mutex id =
    Client.(watch conn [mutex] >> get conn mutex) >>= function
      | Some x when x = id -> Client.(multi conn >> del conn [mutex] >> exec conn) >> IO.return ()
      | _ -> Client.unwatch conn >> IO.fail (Error "lock was lost")

  let with_mutex spec ?atime ?ltime mutex fn =
    let id = Uuidm.(to_bytes (create `V4)) in

    Client.with_connection spec (fun conn ->
      acquire conn ?atime ?ltime mutex id >>
      fn conn >>= fun res ->
      release conn mutex id >>
      IO.return res
    )
end
