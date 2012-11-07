(* error response *)
exception Error of string

module Make(IO : Make.IO)(Client : module type of Client.Make(IO)) : sig
  open Client
  val acquire : connection -> ?atime:float -> ?ltime:int -> string -> string -> unit IO.t
  val release : connection -> string -> string -> unit IO.t
  val with_mutex : connection -> ?atime:float -> ?ltime:int -> string -> (unit -> 'a IO.t) -> 'a IO.t
end
