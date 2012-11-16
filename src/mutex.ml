(* error response *)
exception Error of string

module Make(IO : Make.IO)(Client : module type of Client.Make(IO)) = struct
  open Client

  let (>>=) = IO.(>>=)
  let (>>) x y = x >>= fun _ -> y

  let acquire conn ?(atime=10.) ?(ltime=10) mutex id =
    let etime = Unix.time() +. atime in

    let update_ttl () =
      ttl conn mutex >>= function
        | None -> expire conn mutex ltime >> IO.return ()
        | _ -> IO.return () in

    let rec loop () =
      setnx conn mutex id >>= function
        | true -> expire conn mutex ltime >> IO.return ()
        | _ -> update_ttl () >>
            if Unix.time() < etime then IO.sleep(0.1) >>= loop
            else IO.fail (Error "could not acquire lock")
    in
    loop ()

  let release conn mutex id =
    watch conn [mutex] >> get conn mutex >>= function
      | Some x when x = id ->
          multi conn >> queue (fun () -> del conn [mutex]) >> exec conn >>
          IO.return ()
      | _ -> unwatch conn >> IO.fail (Error "lock was lost")

  let with_mutex conn ?atime ?ltime mutex fn =
    let id = Uuidm.(to_string (create `V4)) in

    acquire conn ?atime ?ltime mutex id >>
    try
      fn () >>= fun res ->
      release conn mutex id >>
      IO.return res
    with e ->
      release conn mutex id >>
      IO.fail e
end
