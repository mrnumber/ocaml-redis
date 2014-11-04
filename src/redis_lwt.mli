open Redis

module IO : sig
  type 'a t = 'a Lwt.t
  type file_descr = Lwt_unix.file_descr
  type in_channel = Lwt_chan.in_channel
  type out_channel = Lwt_chan.out_channel

  type zz = unit
  type 'a stream = 'a Lwt_stream.t

  val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  val catch : (unit -> 'a Lwt.t) -> (exn -> 'a Lwt.t) -> 'a Lwt.t
  val try_bind : (unit -> 'a Lwt.t) -> ('a -> 'b Lwt.t) -> (exn -> 'b Lwt.t) -> 'b Lwt.t
  val ignore_result : 'a Lwt.t -> unit
  val return : 'a -> 'a Lwt.t
  val fail : exn -> 'a Lwt.t

  val socket : Lwt_unix.socket_domain -> Lwt_unix.socket_type -> int -> file_descr
  val connect : file_descr -> Lwt_unix.sockaddr -> unit Lwt.t
  val close : file_descr -> unit Lwt.t
  val sleep : float -> unit Lwt.t

  val in_channel_of_descr : file_descr -> in_channel
  val out_channel_of_descr : file_descr -> out_channel
  val input_char : in_channel -> char Lwt.t
  val really_input : in_channel -> string -> int -> int -> unit Lwt.t
  val output_string : out_channel -> string -> unit Lwt.t
  val flush : out_channel -> unit Lwt.t

  val iter : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
  val iter_serial : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
  val map : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
  val map_serial : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
  val fold_left : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

  val stream_from : (zz -> 'b option t) -> 'b Lwt_stream.t
  val stream_next: 'a Lwt_stream.t -> 'a Lwt.t
end

module Client : module type of Client.Make(IO)
module Cache : module type of Cache.Make(IO)(Client)
module Mutex : module type of Mutex.Make(IO)(Client)
