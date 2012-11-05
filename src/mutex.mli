(* error response *)
exception Error of string

module Make(IO : Make.IO)(Client : module type of Client.Make(IO)) : sig
  val acquire : Client.connection -> ?atime:float -> ?ltime:int -> string -> string -> unit IO.t
  val release : Client.connection -> string -> string -> unit IO.t
  val with_mutex : Client.connection -> ?atime:float -> ?ltime:int -> string -> (unit -> 'a IO.t) -> 'a IO.t
end
