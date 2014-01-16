(** Bindings for redis.

    This has only been tested with Redis 2.2, but will probably work for >= 2.0
 **)

(* Make communication module *)
module Make(IO : Make.IO) : sig
  type connection = private {
    fd     : IO.file_descr;
    in_ch  : IO.in_channel;
    out_ch : IO.out_channel;
  }

  (* reply from server *)
  type reply = [
    | `Status of string
    | `Error of string
    | `Int of int
    | `Int64 of Int64.t
    | `Bulk of string option
    | `Multibulk of reply list
  ]

  (* error responses from server *)
  exception Error of string

  (* these signal protocol errors *)
  exception Unexpected of reply
  exception Unrecognized of string * string (* explanation, data *)

  (* server connection info *)
  type connection_spec = {
    host : string;
    port : int;
  }

  val connect : connection_spec -> connection IO.t
  val disconnect : connection -> unit IO.t
  val with_connection : connection_spec -> (connection -> 'a IO.t) -> 'a IO.t

  (* Raises Error if password is invalid. *)
  val auth : connection -> string -> unit IO.t

  val echo : connection -> string -> string option IO.t
  val ping : connection -> bool IO.t
  val quit : connection -> unit IO.t

  (* Switch to a different db; raises Error if index is invalid. *)
  val select : connection -> int -> unit IO.t

  (** Generic key commands *)

  (* Returns the number of keys removed. *)
  val del : connection -> string list -> int IO.t

  val exists : connection -> string -> bool IO.t

  (* Returns the number of keys removed. *)
  val expire : connection -> string -> int -> bool IO.t

  (* Like "expire" but with absolute (Unix) time; the time is truncated to the nearest second. *)
  val expireat : connection -> string -> float -> bool IO.t

  (* Probably not a good idea to use this in production; see Redis documentation. *)
  val keys : connection -> string -> string list IO.t

  (* Move key to a different db; returns true if key was moved, false otherwise. *)
  val move : connection -> string -> int -> bool IO.t

  (* Remove timeout on key; returns true if timeout was removed, false otherwise. *)
  val persist : connection -> string -> bool IO.t

  (* returns none if db is empty. *)
  val randomkey : connection -> string option IO.t

  (* Raises Error if key doesn't exist. *)
  val rename : connection -> string -> string -> unit IO.t

  (* Raises Error if key doesn't exist; returns true if key was renamed, false if newkey already exists. *)
  val renamenx : connection -> string -> string -> bool IO.t

  val sort :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] -> ?alpha:bool -> string -> string list IO.t

  val sort_and_store :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] ->
    ?alpha:bool -> string -> string -> int IO.t

  (* Returns None if key doesn't exist or doesn't have a timeout. *)
  val ttl : connection -> string -> int option IO.t

  (* TYPE is a reserved word in ocaml *)
  val type_of : connection -> string -> [> `Hash | `List | `None | `String | `Zset ] IO.t

  (** String commands *)

  (* Returns length of string after append. *)
  val append : connection -> string -> string -> int IO.t

  val decr : connection -> string -> int IO.t

  val decrby : connection -> string -> int -> int IO.t

  val get : connection -> string -> string option IO.t

  (* Out of range offsets will return 0. *)
  val getbit : connection -> string -> int -> int IO.t

  (* Out of range arguments are handled by limiting to valid range. *)
  val getrange : connection -> string -> int -> int -> string option IO.t

  (* Set value and return old value. Raises Error when key exists but isn't a string. *)
  val getset : connection -> string -> string -> string option IO.t

  val incr : connection -> string -> int IO.t

  val incrby : connection -> string -> int -> int IO.t

  val mget : connection -> string list -> string option list IO.t

  (* This is atomic: either all keys are set or none are. *)
  val mset : connection -> (string * string) list -> unit IO.t

  (* Like MSET, this is atomic. If even a single key exists, no operations will be performed.
     Returns true if all keys were set, false otherwise. *)
  val msetnx : connection -> (string * string) list -> bool IO.t

  val set : connection -> string -> string -> unit IO.t

  (* Returns the original bit value. *)
  val setbit : connection -> string -> int -> int -> int IO.t

  val setex : connection -> string -> int -> string -> unit IO.t

  (* Returns true if key was set, false otherwise. *)
  val setnx : connection -> string -> string -> bool IO.t

  (* If offset > length, string will be padded with 0-bytes. Returns length of string after modification. *)
  val setrange : connection -> string -> int -> string -> int IO.t

  val strlen : connection -> string -> int IO.t

  (** Hash commands *)

  (* Returns true if field exists and was deleted, false otherwise. *)
  val hdel : connection -> string -> string -> bool IO.t

  val hexists : connection -> string -> string -> bool IO.t

  val hget : connection -> string -> string -> string option IO.t

  val hgetall : connection -> string -> (string * string) list IO.t

  (* Raises error if field already contains a non-numeric value. *)
  val hincrby : connection -> string -> string -> int -> int IO.t

  val hkeys : connection -> string -> string list IO.t

  val hlen : connection -> string -> int IO.t

  val hmget : connection -> string -> string list -> string option list IO.t

  val hmset : connection -> string -> (string * string) list -> unit IO.t

  (* Returns true if field was added, false otherwise. *)
  val hset : connection -> string -> string -> string -> bool IO.t

  (* Returns true if field was set, false otherwise. *)
  val hsetnx : connection -> string -> string -> string -> bool IO.t

  val hvals : connection -> string -> string list IO.t

  (** List commands *)

  (* Blocks while all of the lists are empty. Set timeout to number of seconds OR 0 to block indefinitely. *)
  val blpop : connection -> string list -> int -> (string * string) option IO.t

  (* Same as BLPOP except pulling the last instead of first element. *)
  val brpop : connection -> string list -> int -> (string * string) option IO.t

  (* Blocking RPOPLPUSH.  Returns None on timeout. *)
  val brpoplpush : connection -> string -> string -> int -> string option IO.t

  (* Out of range or nonexistent key will return None. *)
  val lindex : connection -> string -> int -> string option IO.t

  (* Returns None if pivot isn't found, otherwise returns length of list after insert. *)
  val linsert : connection -> string -> [< `After | `Before ] -> string -> string -> int option IO.t

  val llen : connection -> string -> int IO.t

  val lpop : connection -> string -> string option IO.t

  (* Returns length of list after operation. *)
  val lpush : connection -> string -> string -> int IO.t

  (* Only push when list exists. Return length of list after operation. *)
  val lpushx : connection -> string -> string -> int IO.t

  (* Out of range arguments are handled by limiting to valid range. *)
  val lrange : connection -> string -> int -> int -> string list IO.t

  (* Returns number of elements removed. *)
  val lrem : connection -> string -> int -> string -> int IO.t

  (* Raises Error if out of range. *)
  val lset : connection -> string -> int -> string -> unit IO.t

  (* Removes all but the specified range. Out of range arguments are handled by limiting to valid range. *)
  val ltrim : connection -> string -> int -> int -> unit IO.t

  val rpop : connection -> string -> string option IO.t

  (* Remove last element of source and insert as first element of destination. Returns the element moved
     or None if source is empty. *)
  val rpoplpush : connection -> string -> string -> string option IO.t

  (* Returns length of list after operation. *)
  val rpush : connection -> string -> string -> int IO.t

  val rpushx : connection -> string -> string -> int IO.t

  (** Set commands *)

  (* Returns true if member was added, false otherwise. *)
  val sadd : connection -> string -> string -> bool IO.t

  val scard : connection -> string -> int IO.t

  (* Difference between first and all successive sets. *)
  val sdiff : connection -> string list -> string list IO.t

  (* like sdiff, but store result in destination. returns size of result. *)
  val sdiffstore : connection -> string -> string list -> int IO.t

  val sinter : connection -> string list -> string list IO.t

  (* Like SINTER, but store result in destination. Returns size of result. *)
  val sinterstore : connection -> string -> string list -> int IO.t

  val sismember : connection -> string -> string -> bool IO.t

  val smembers : connection -> string -> string list IO.t

  (* Returns true if an element was moved, false otherwise. *)
  val smove : connection -> string -> string -> string -> bool IO.t

  (* Remove random element from set. *)
  val spop : connection -> string -> string option IO.t

  (* Like SPOP, but doesn't remove chosen element. *)
  val srandmember : connection -> string -> string option IO.t

  (* Returns true if element was removed. *)
  val srem : connection -> string -> string -> bool IO.t

  val sunion : connection -> string list -> string list IO.t

  (* Like SUNION, but store result in destination. Returns size of result. *)
  val sunionstore : connection -> string -> string list -> int IO.t

  (** Pub/sub commands *)

  (* Post a message to a channel. Returns number of clients that received the message. *)
  val publish : connection -> string -> string -> int IO.t

  (* Lists the currently active channels. If no pattern is specified, all channels are listed. *)
  val pubsub_channels : connection -> string option -> reply list IO.t

  (** Transaction commands *)

  (* Marks the start of a transaction block. Subsequent commands will be queued for atomic execution using EXEC. *)
  val multi : connection -> unit IO.t

  (* Executes all previously queued commands in a transaction and restores the connection state to normal. *)
  val exec : connection -> reply list IO.t

  (* Flushes all previously queued commands in a transaction and restores the connection state to normal. *)
  val discard : connection -> unit IO.t

  (* Marks the given keys to be watched for conditional execution of a transaction. *)
  val watch : connection -> string list -> unit IO.t

  (* Flushes all the previously watched keys for a transaction. *)
  val unwatch : connection -> unit IO.t

  val queue : (unit -> 'a IO.t) -> unit IO.t

  (** Server *)

  val bgrewriteaof : connection -> unit IO.t

  val bgsave : connection -> unit IO.t

  val config_resetstat : connection -> unit IO.t

  val dbsize : connection -> int IO.t

  (* clear all databases *)
  val flushall : connection -> unit IO.t

  (* clear current database *)
  val flushdb : connection -> unit IO.t

  val info : connection -> (string * string) list IO.t

  (* last successful save as Unix timestamp *)
  val lastsave : connection -> float IO.t

  (* synchronous save *)
  val save : connection -> unit IO.t

  (* save and shutdown server *)
  val shutdown : connection -> unit IO.t
end
