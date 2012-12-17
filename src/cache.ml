(** A convenience module for writing redis caches *)
open Batteries

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
module Make(IO : Make.IO)(Client : module type of Client.Make(IO))(S : S) = struct
  open Client

  let (>>=) = IO.(>>=)

  let set r key data =
    let key = S.cache_key key in
    let data = S.string_of_data data in
    set r key data >>= fun () ->
    IO.return (Option.may
      (fun cache_expiration ->
        IO.ignore_result (expire r key cache_expiration)
      )
      S.cache_expiration)

  let get r key =
    let key = S.cache_key key in
    get r key >>= fun value ->
    IO.return (Option.map S.data_of_string value)

  let delete r key =
    let key = S.cache_key key in
    IO.ignore_result (del r [key])
end
