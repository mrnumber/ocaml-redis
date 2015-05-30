(** Bindings for redis.

    This has only been tested with Redis 2.2, but will probably work for >= 2.0
 **)

(* Make communication module *)
module Make(IO : S.IO) : S.Client with module IO = IO
