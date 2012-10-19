(** Bindings for redis.

    This has only been tested with Redis 2.2, but will probably work for >= 2.0
 **)

(* reply from server *)
type reply = [
  | `Status of string
  | `Error of string
  | `Int of int
  | `Int64 of Int64.t
  | `Bulk of string option
  | `Multibulk of string option list
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

(* Make communication module *)
module Make(A : sig
  type 'a _r
  type file_descr
  type in_channel
  type out_channel

  (* Lwt stuff *)
  val (>>=)         : 'a _r -> ('a -> 'b _r) -> 'b _r
  val try_bind      : (unit -> 'a _r) -> ('a -> 'b _r) -> (exn -> 'b _r) -> 'b _r
  val return        : 'a -> 'a _r
  val fail          : exn -> 'a _r

  (* Lwt_unix stuff *)
  val socket  : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  val connect : file_descr -> Unix.sockaddr -> unit _r
  val close   : file_descr -> unit

  (* Lwt_chan stuff *)
  val in_channel_of_descr  : file_descr -> in_channel
  val out_channel_of_descr : file_descr -> out_channel
  val input_char           : in_channel -> char _r
  val really_input         : in_channel -> string -> int -> int -> unit _r
  val output_string        : out_channel -> string -> unit _r
  val flush                : out_channel -> unit _r

  (* Lwt_util stuff *)
  val iter : ('a -> unit _r) -> 'a list -> unit _r
end) :
sig
  type connection

  val connect : connection_spec -> connection A._r
  val disconnect : connection -> unit
  val with_connection : connection_spec -> (connection -> 'a A._r) -> 'a A._r

  (* Raises Error if password is invalid. *)
  val auth : connection -> string -> unit A._r

  val echo : connection -> string -> string option A._r
  val ping : connection -> unit A._r
  val quit : connection -> unit A._r

  (* Switch to a different db; raises Error if index is invalid. *)
  val select : connection -> int -> unit A._r

  (** Generic key commands *)

  (* Returns the number of keys removed. *)
  val del : connection -> string list -> int A._r

  val exists : connection -> string -> bool A._r

  (* Returns the number of keys removed. *)
  val expire : connection -> string -> int -> bool A._r

  (* Like "expire" but with absolute (Unix) time; the time is truncated to the nearest second. *)
  val expireat : connection -> string -> float -> bool A._r

  (* Probably not a good idea to use this in production; see Redis documentation. *)
  val keys : connection -> string -> string list A._r

  (* Move key to a different db; returns true if key was moved, false otherwise. *)
  val move : connection -> string -> int -> bool A._r

  (* Remove timeout on key; returns true if timeout was removed, false otherwise. *)
  val persist : connection -> string -> bool A._r

  (* returns none if db is empty. *)
  val randomkey : connection -> string option A._r

  (* Raises Error if key doesn't exist. *)
  val rename : connection -> string -> string -> unit A._r

  (* Raises Error if key doesn't exist; returns true if key was renamed, false if newkey already exists. *)
  val renamenx : connection -> string -> string -> bool A._r

  val sort :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] -> ?alpha:bool -> string -> string list A._r

  val sort_and_store :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] ->
    ?alpha:bool -> string -> string -> int A._r

  (* Returns None if key doesn't exist or doesn't have a timeout. *)
  val ttl : connection -> string -> int option A._r

  (* TYPE is a reserved word in ocaml *)
  val type_of : connection -> string -> [> `Hash | `List | `None | `String | `Zset ] A._r

  (** String commands *)

  (* Returns length of string after append. *)
  val append : connection -> string -> string -> int A._r

  val decr : connection -> string -> int A._r

  val decrby : connection -> string -> int -> int A._r

  val get : connection -> string -> string option A._r

  (* Out of range offsets will return 0. *)
  val getbit : connection -> string -> int -> int A._r

  (* Out of range arguments are handled by limiting to valid range. *)
  val getrange : connection -> string -> int -> int -> string option A._r

  (* Set value and return old value. Raises Error when key exists but isn't a string. *)
  val getset : connection -> string -> string -> string option A._r

  val incr : connection -> string -> int A._r

  val incrby : connection -> string -> int -> int A._r

  val mget : connection -> string list -> string option list A._r

  (* This is atomic: either all keys are set or none are. *)
  val mset : connection -> (string * string) list -> unit A._r

  (* Like MSET, this is atomic. If even a single key exists, no operations will be performed.
     Returns true if all keys were set, false otherwise. *)
  val msetnx : connection -> (string * string) list -> bool A._r

  val set : connection -> string -> string -> unit A._r

  (* Returns the original bit value. *)
  val setbit : connection -> string -> int -> int -> int A._r

  val setex : connection -> string -> int -> string -> unit A._r

  (* Returns true if key was set, false otherwise. *)
  val setnx : connection -> string -> string -> bool A._r

  (* If offset > length, string will be padded with 0-bytes. Returns length of string after modification. *)
  val setrange : connection -> string -> int -> string -> int A._r

  val strlen : connection -> string -> int A._r

  (** Hash commands *)

  (* Returns true if field exists and was deleted, false otherwise. *)
  val hdel : connection -> string -> string -> bool A._r

  val hexists : connection -> string -> string -> bool A._r

  val hget : connection -> string -> string -> string option A._r

  val hgetall : connection -> string -> (string * string) list A._r

  (* Raises error if field already contains a non-numeric value. *)
  val hincrby : connection -> string -> string -> int -> int A._r

  val hkeys : connection -> string -> string list A._r

  val hlen : connection -> string -> int A._r

  val hmget : connection -> string -> string list -> string option list A._r

  val hmset : connection -> string -> (string * string) list -> unit A._r

  (* Returns true if field was added, false otherwise. *)
  val hset : connection -> string -> string -> string -> bool A._r

  (* Returns true if field was set, false otherwise. *)
  val hsetnx : connection -> string -> string -> string -> bool A._r

  val hvals : connection -> string -> string list A._r

  (** List commands *)

  (* Blocks while all of the lists are empty. Set timeout to number of seconds OR 0 to block indefinitely. *)
  val blpop : connection -> string list -> int -> (string * string) option A._r

  (* Same as BLPOP except pulling the last instead of first element. *)
  val brpop : connection -> string list -> int -> (string * string) option A._r

  (* Blocking RPOPLPUSH.  Returns None on timeout. *)
  val brpoplpush : connection -> string -> string -> int -> string option A._r

  (* Out of range or nonexistent key will return None. *)
  val lindex : connection -> string -> int -> string option A._r

  (* Returns None if pivot isn't found, otherwise returns length of list after insert. *)
  val linsert : connection -> string -> [< `After | `Before ] -> string -> string -> int option A._r

  val llen : connection -> string -> int A._r

  val lpop : connection -> string -> string option A._r

  (* Returns length of list after operation. *)
  val lpush : connection -> string -> string -> int A._r

  (* Only push when list exists. Return length of list after operation. *)
  val lpushx : connection -> string -> string -> int A._r

  (* Out of range arguments are handled by limiting to valid range. *)
  val lrange : connection -> string -> int -> int -> string list A._r

  (* Returns number of elements removed. *)
  val lrem : connection -> string -> int -> string -> int A._r

  (* Raises Error if out of range. *)
  val lset : connection -> string -> int -> string -> unit A._r

  (* Removes all but the specified range. Out of range arguments are handled by limiting to valid range. *)
  val ltrim : connection -> string -> int -> int -> unit A._r

  val rpop : connection -> string -> string option A._r

  (* Remove last element of source and insert as first element of destination. Returns the element moved
     or None if source is empty. *)
  val rpoplpush : connection -> string -> string -> string option A._r

  (* Returns length of list after operation. *)
  val rpush : connection -> string -> string -> int A._r

  val rpushx : connection -> string -> string -> int A._r

  (** Set commands *)

  (* Returns true if member was added, false otherwise. *)
  val sadd : connection -> string -> string -> bool A._r

  val scard : connection -> string -> int A._r

  (* Difference between first and all successive sets. *)
  val sdiff : connection -> string list -> string list A._r

  (* like sdiff, but store result in destination. returns size of result. *)
  val sdiffstore : connection -> string -> string list -> int A._r

  val sinter : connection -> string list -> string list A._r

  (* Like SINTER, but store result in destination. Returns size of result. *)
  val sinterstore : connection -> string -> string list -> int A._r

  val sismember : connection -> string -> string -> bool A._r

  val smembers : connection -> string -> string list A._r

  (* Returns true if an element was moved, false otherwise. *)
  val smove : connection -> string -> string -> string -> bool A._r

  (* Remove random element from set. *)
  val spop : connection -> string -> string option A._r

  (* Like SPOP, but doesn't remove chosen element. *)
  val srandmember : connection -> string -> string option A._r

  (* Returns true if element was removed. *)
  val srem : connection -> string -> string -> bool A._r

  val sunion : connection -> string list -> string list A._r

  (* Like SUNION, but store result in destination. Returns size of result. *)
  val sunionstore : connection -> string -> string list -> int A._r

  (** Server *)

  val bgrewriteaof : connection -> unit A._r

  val bgsave : connection -> unit A._r

  val config_resetstat : connection -> unit A._r

  val dbsize : connection -> int A._r

  (* clear all databases *)
  val flushall : connection -> unit A._r

  (* clear current database *)
  val flushdb : connection -> unit A._r

  val info : connection -> (string * string) list A._r

  (* last successful save as Unix timestamp *)
  val lastsave : connection -> float A._r

  (* synchronous save *)
  val save : connection -> unit A._r

  (* save and shutdown server *)
  val shutdown : connection -> unit A._r
end
