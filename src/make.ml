module type IO = sig
  type 'a t
  type file_descr
  type in_channel
  type out_channel

  type zz
  type 'a stream

  (* Lwt stuff *)
  val (>>=)         : 'a t -> ('a -> 'b t) -> 'b t
  val catch         : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val try_bind      : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
  val ignore_result : 'a t -> unit
  val return        : 'a -> 'a t
  val fail          : exn -> 'a t

  (* Lwt_unix stuff *)
  val socket  : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  val connect : file_descr -> Unix.sockaddr -> unit t
  val close   : file_descr -> unit t
  val sleep   : float -> unit t

  (* Lwt_chan stuff *)
  val in_channel_of_descr  : file_descr -> in_channel
  val out_channel_of_descr : file_descr -> out_channel
  val input_char           : in_channel -> char t
  val really_input         : in_channel -> string -> int -> int -> unit t
  val output_string        : out_channel -> string -> unit t
  val flush                : out_channel -> unit t

  (* Lwt_util stuff *)
  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter_serial : ('a -> unit t) -> 'a list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val map_serial : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  val stream_from : (zz -> 'b option t) -> 'b stream
  val stream_next : 'a stream -> 'a t
end
