module IO : sig
  type 'a t = 'a
  type file_descr = Unix.file_descr
  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel

  type zz = int
  type 'a stream = 'a Stream.t

  val (>>=) : 'a -> ('a -> 'b) -> 'b
  val catch : (unit -> 'a) -> (exn -> 'a) -> 'a
  val try_bind : (unit -> 'a) -> ('a -> 'b) -> (exn -> 'b) -> 'b
  val ignore_result : 'a -> unit
  val return : 'a -> 'a
  val fail : exn -> 'a

  val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  val connect : file_descr -> Unix.sockaddr -> unit
  val close : file_descr -> unit
  val sleep : float -> unit

  val in_channel_of_descr : file_descr -> in_channel
  val out_channel_of_descr : file_descr -> out_channel
  val input_char : in_channel -> char
  val really_input : in_channel -> string -> int -> int -> unit
  val output_string : out_channel -> string -> unit
  val flush : out_channel -> unit

  val iter : ('a -> unit) -> 'a list -> unit
  val iter_serial : ('a -> unit t) -> 'a list -> unit
  val map : ('a -> 'b t) -> 'a list -> 'b list
  val map_serial : ('a -> 'b t) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a

  val stream_from : (zz -> 'b option t) -> 'b Stream.t
  val stream_next : 'a Stream.t -> 'a
end

module Client : module type of Client.Make(IO)
module Cache : module type of Cache.Make(IO)(Client)
module Mutex : module type of Mutex.Make(IO)(Client)
