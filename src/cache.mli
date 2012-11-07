(** A convenience module for writing redis caches *)

(** Signature to parameterize the cache *)
module type S = sig
  type key
  type data

  val cache_key : key -> string
  val cache_expiration : int option

  val data_of_string : string -> data
  val string_of_data : data -> string
end

(** Make a redis_cache *)
module Make(IO : Make.IO)(Client : module type of Client.Make(IO))(S : S) : sig
  open Client
  val set : connection -> S.key -> S.data -> unit IO.t
  val get : connection -> S.key -> S.data option IO.t
  val delete : connection -> S.key -> unit
end
