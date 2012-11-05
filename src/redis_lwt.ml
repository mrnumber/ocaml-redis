module IO = struct
  type 'a t = 'a Lwt.t
  type file_descr = Lwt_unix.file_descr
  type in_channel = Lwt_chan.in_channel
  type out_channel = Lwt_chan.out_channel

  let (>>=) = Lwt.(>>=)
  let catch = Lwt.catch
  let try_bind = Lwt.try_bind
  let ignore_result = Lwt.ignore_result
  let return = Lwt.return
  let fail = Lwt.fail

  let socket = Lwt_unix.socket
  let connect = Lwt_unix.connect
  let close = Lwt_unix.close
  let sleep = Lwt_unix.sleep

  let in_channel_of_descr = Lwt_chan.in_channel_of_descr
  let out_channel_of_descr = Lwt_chan.out_channel_of_descr
  let input_char = Lwt_chan.input_char
  let really_input = Lwt_chan.really_input
  let output_string = Lwt_chan.output_string
  let flush = Lwt_chan.flush

  let iter = Lwt_util.iter
  let iter_serial = Lwt_util.iter_serial
  let map = Lwt_util.map
  let map_serial = Lwt_util.map_serial
  let fold_left = Lwt_util.fold_left
end

module Client = Client.Make(IO)
module Cache = Cache.Make(IO)(Client)
module Mutex = Mutex.Make(IO)(Client)
