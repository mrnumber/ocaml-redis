open Redis

module IO = struct
  type 'a t = 'a Lwt.t
  type file_descr = Lwt_unix.file_descr
  type in_channel = Lwt_chan.in_channel
  type out_channel = Lwt_chan.out_channel

  type zz = unit
  type 'a stream = 'a Lwt_stream.t

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

  let iter = Lwt_list.iter_p
  let iter_serial = Lwt_list.iter_s
  let map = Lwt_list.map_p
  let map_serial = Lwt_list.map_s
  let fold_left = Lwt_list.fold_left_s

  let stream_from = Lwt_stream.from
  let stream_next = Lwt_stream.next
end

module Client = Client.Make(IO)
module Cache = Cache.Make(IO)(Client)
module Mutex = Mutex.Make(IO)(Client)
