module IO = struct
  type 'a t = 'a
  type file_descr = Unix.file_descr
  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel

  let (>>=) a f = f a
  let catch f exn_handler = try f () with e -> exn_handler e
  let try_bind f bind_handler exn_handler = try f () >>= bind_handler with e -> exn_handler e
  let ignore_result = ignore
  let return a = a
  let fail e = raise e

  let socket = Unix.socket
  let connect = Unix.connect
  let close = Unix.close
  let sleep a = ignore (Unix.select [] [] [] a)

  let in_channel_of_descr = Unix.in_channel_of_descr
  let out_channel_of_descr = Unix.out_channel_of_descr
  let input_char = Pervasives.input_char
  let really_input = Pervasives.really_input
  let output_string = output_string
  let flush = Pervasives.flush

  let iter = List.iter
  let iter_serial = List.iter
  let map = List.map
  let map_serial = List.map
  let fold_left = List.fold_left
end

module Client = Client.Make(IO)
module Cache = Cache.Make(IO)(Client)
module Mutex = Mutex.Make(IO)(Client)
