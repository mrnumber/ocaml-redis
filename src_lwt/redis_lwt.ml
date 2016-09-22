module IO = struct
  type 'a t = 'a Lwt.t

  type fd = Lwt_unix.file_descr
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  type 'a stream = 'a Lwt_stream.t
  type stream_count = unit

  let (>>=) = Lwt.(>>=)
  let catch = Lwt.catch
  let try_bind = Lwt.try_bind
  let ignore_result = Lwt.ignore_result
  let return = Lwt.return
  let fail = Lwt.fail
  let run = Lwt_main.run

  let connect host port =
    let fd = Lwt_unix.socket (Unix.PF_INET) Unix.SOCK_STREAM 0 in
    let do_connect () =
      let port = string_of_int port in
      Lwt_unix.getaddrinfo host port [] >>= function
      | [] -> Lwt.fail_with "Could not resolve redis host!"
      | addrinfo::_ -> return addrinfo.Lwt_unix.ai_addr >>= fun sock_addr ->
      Lwt_unix.connect fd sock_addr >>= fun () ->
      return fd
    in
    catch do_connect (fun exn -> Lwt_unix.close fd >>= fun () -> fail exn)

  let close = Lwt_unix.close
  let sleep = Lwt_unix.sleep

  let in_channel_of_descr fd = Lwt_io.of_fd ~mode:Lwt_io.input fd
  let out_channel_of_descr fd = Lwt_io.of_fd ~mode:Lwt_io.output fd
  let input_char = Lwt_io.read_char
  let really_input = Lwt_io.read_into_exactly
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush

  let iter = Lwt_list.iter_p
  let iter_serial = Lwt_list.iter_s
  let map = Lwt_list.map_p
  let map_serial = Lwt_list.map_s
  let fold_left = Lwt_list.fold_left_s

  let stream_from = Lwt_stream.from
  let stream_next = Lwt_stream.next
end

module Client = Redis.Client.Make(IO)
module Cache = Redis.Cache.Make(IO)(Client)
module Mutex = Redis.Mutex.Make(IO)(Client)
