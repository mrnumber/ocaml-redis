(** Bindings for redis.

    This has only been tested with Redis 2.2, but will probably work for >= 2.0
 **)
open Batteries

(* Make communication module *)
module Make(IO : Make.IO) = struct
  let (>>=) = IO.(>>=)

  type connection = {
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

  let write out_ch args =
    let num_args = List.length args in
    IO.output_string out_ch (Printf.sprintf "*%d" num_args) >>= fun () ->
    IO.output_string out_ch "\r\n" >>= fun () ->
    IO.iter
      (fun arg ->
        let length = String.length arg in
        IO.output_string out_ch (Printf.sprintf "$%d" length) >>= fun () ->
        IO.output_string out_ch "\r\n" >>= fun () ->
        IO.output_string out_ch arg >>= fun () ->
        IO.output_string out_ch "\r\n"
      )
      args >>= fun () ->
    IO.flush out_ch

  let read_fixed_line length in_ch =
    let line = String.create length in
    IO.really_input in_ch line 0 length >>= fun () ->
    IO.input_char in_ch >>= fun c1 ->
    IO.input_char in_ch >>= fun c2 ->
    match c1, c2 with
      | '\r', '\n' -> IO.return line
      | _          -> IO.fail (Unrecognized ("Expected terminator", line))

  let read_line in_ch =
    let buf = Buffer.create 32 in
    let rec loop () =
      IO.input_char in_ch >>= function
        | '\r' ->
            IO.input_char in_ch >>= (function
              | '\n' ->
                  IO.return (Buffer.contents buf)
              | c ->
                  Buffer.add_char buf '\r';
                  Buffer.add_char buf c;
                  loop ()
            )
        | c ->
            Buffer.add_char buf c;
            loop ()
    in
    loop ()

  (* this expects the initial ':' to have already been consumed *)
  let read_integer in_ch =
    read_line in_ch >>= fun line ->
    IO.return
      (try `Int (int_of_string line)
       with _ -> `Int64 (Int64.of_string line))

  (* this expects the initial '$' to have already been consumed *)
  let read_bulk in_ch =
    read_line in_ch >>= fun line ->
      match int_of_string line with
        | -1 -> IO.return (`Bulk None)
        | n when n >= 0 ->
            read_fixed_line n in_ch >>= fun data ->
            IO.return (`Bulk (Some data))
        | n ->
            IO.fail (Unrecognized ("Invalid bulk length", string_of_int n))

  (* this expects the initial '*' to have already been consumed *)
  let rec read_multibulk in_ch =
    let rec loop acc n =
      if n <= 0 then
        IO.return (`Multibulk (List.rev acc))
      else
        read_reply in_ch >>= fun data -> loop (data :: acc) (n - 1)
    in
    read_line in_ch >>= fun line ->
    let num_bulk = int_of_string line in
    loop [] num_bulk

  and read_reply in_ch =
    IO.input_char in_ch >>= function
      | '+' ->
          read_line in_ch >>= fun s -> IO.return (`Status s)
      | '-' ->
          read_line in_ch >>= fun s -> IO.return (`Error s)
      | ':' ->
          read_integer in_ch
      | '$' ->
          read_bulk in_ch
      | '*' ->
          read_multibulk in_ch
      | c ->
          IO.fail (Unrecognized ("Unexpected char in reply", Char.escaped c))

  let read_reply_exn in_ch =
    read_reply in_ch >>= function
      | `Status _
      | `Int _
      | `Int64 _
      | `Bulk _
      | `Multibulk _ as reply ->
          IO.return reply
      | `Error msg ->
          IO.fail (Error msg)

  let send_request connection command =
    write connection.out_ch command >>= fun () ->
    read_reply_exn connection.in_ch

  let interleave list =
    let rec loop acc = function
      | (x, y) :: tail -> loop (y :: x :: acc ) tail
      | [] -> List.rev acc
    in
    loop [] list

  (* this assumes the list length is even *)
  let deinterleave list =
    let rec loop acc = function
      | x :: y :: tail -> loop ((x, y) :: acc) tail
      | [] -> List.rev acc
      | _ -> raise (Invalid_argument "List length must be even")
    in
    loop [] list

  let return_bulk = function
    | `Bulk b -> IO.return b
    | x       -> IO.fail (Unexpected x)

  let return_bool = function
    | `Int 0 -> IO.return false
    | `Int 1 -> IO.return true
    | x      -> IO.fail (Unexpected x)

  let return_status = function
    | `Status _ as s -> IO.return s
    | x              -> IO.fail (Unexpected x)

  let return_expected_status expected = function
    | `Status s when expected = s -> IO.return ()
    | x                           -> IO.fail (Unexpected x)

  let return_ok_status =
    return_expected_status "OK"

  let return_queued_status =
    return_expected_status "QUEUED"

  let return_int = function
    | `Int n -> IO.return n
    | x      -> IO.fail (Unexpected x)

  let return_int64 = function
    | `Int n   -> IO.return (Int64.of_int n)
    | `Int64 n -> IO.return n
    | x        -> IO.fail (Unexpected x)

  let return_float = function
    | `Int n   -> IO.return (float_of_int n)
    | `Int64 n -> IO.return (Int64.to_float n)
    | x        -> IO.fail (Unexpected x)

  let return_multibulk = function
    | `Multibulk m -> IO.return m
    | x            -> IO.fail (Unexpected x)

  let return_bulk_multibulk reply =
    try
      return_multibulk reply >>= fun list ->
      IO.return (List.map (function
        | `Bulk b -> b
        | x -> raise (Unexpected x)
      ) list)
    with e -> IO.fail e

  (* multibulks all of whose entries are not nil *)
  let return_no_nil_multibulk reply =
    return_bulk_multibulk reply >>= fun list ->
    IO.return (List.filter_map identity list)

  let return_key_value_multibulk reply =
    return_bulk_multibulk reply >>= fun list ->
      try
        IO.return (List.filter_map (function
          | (Some k, Some v) -> Some (k, v)
          | _ -> None
        ) (deinterleave list))
      with e -> IO.fail e

  let return_opt_pair_multibulk reply =
    return_bulk_multibulk reply >>= function
      | []               -> IO.return None
      | [Some x; Some y] -> IO.return (Some (x, y))
      | x                -> IO.fail (Invalid_argument "Expected nil or two-element multi-bulk")

  let return_info_bulk reply =
    return_bulk reply >>= function
      | Some b ->
          let fields = String.nsplit b "\r\n" in
          let fields = List.filter (fun x -> x <> "") fields in
          IO.return (List.map (fun f -> String.split f ":") fields)
      | None   -> IO.return []

  (* generate command for SORT *)
  let sort_command
      ?by
      ?limit (* offset, limit *)
      ?(get=[])
      ?order
      ?(alpha=false)
      ?store
      key =
    let command = ref [ key; "SORT" ] in (* we'll reverse this later *)
    let append x = command := x :: !command in
    (match by with
       | Some by ->
           append "BY";
           append by
       | None ->
           ()
    );
    (match limit with
       | Some (offset, limit) ->
           append "LIMIT";
           append (string_of_int offset);
           append (string_of_int limit);
       | None ->
           ()
    );
    (match order with
       | Some `Asc -> append "ASC"
       | Some `Desc -> append "DESC"
       | None -> ()
    );
    if alpha then append "ALPHA";
    (match store with
       | Some dest ->
           append "STORE";
           append dest
       | None ->
           ()
    );
    List.rev !command

  let connect spec =
    let s = IO.socket (Unix.PF_INET) Unix.SOCK_STREAM 0 in
    let sock_addr =
      let inet_addr = Unix.inet_addr_of_string spec.host in
      Unix.ADDR_INET (inet_addr, spec.port)
    in
    IO.connect s sock_addr >>= fun () ->
    IO.return
      { fd = s;
        in_ch = IO.in_channel_of_descr s;
        out_ch = IO.out_channel_of_descr s;
      }

  let disconnect connection =
    (* both channels are bound to the same file descriptor so we only need
       to close one of them *)
    IO.close connection.fd

  let with_connection spec f =
    connect spec >>= fun c ->
    try
      f c >>= fun r ->
      disconnect c >>= fun () ->
      IO.return r
    with e ->
      disconnect c >>= fun () ->
      IO.fail e

  (* Raises Error if password is invalid. *)
  let auth connection password =
    let command = [ "AUTH"; password ] in
    send_request connection command >>= return_ok_status

  let echo connection message =
    let command = [ "ECHO"; message ] in
    send_request connection command >>= return_bulk

  let ping connection =
    let command = [ "PING" ] in
    IO.try_bind
      (fun () -> send_request connection command)
      (function `Status "PONG" -> IO.return true | _ -> IO.return false)
      (fun _ -> IO.return false)

  let quit connection =
    let command = [ "QUIT" ] in
    send_request connection command >>= return_ok_status

  (* Switch to a different db; raises Error if index is invalid. *)
  let select connection index =
    let index = string_of_int index in
    let command = [ "SELECT"; index ] in
    send_request connection command >>= return_ok_status

  (** Generic key commands *)

  (* Returns the number of keys removed. *)
  let del connection keys =
    let command = "DEL" :: keys in
    send_request connection command >>= return_int

  let exists connection key =
    let command = [ "EXISTS"; key ] in
    send_request connection command >>= return_bool

  (* Returns true if timeout was set, false otherwise. *)
  let expire connection key seconds =
    let seconds = string_of_int seconds in
    let command = [ "EXPIRE"; key; seconds ] in
    send_request connection command >>= return_bool

  (* Like "expire" but with absolute (Unix) time; the time is truncated to the nearest second. *)
  let expireat connection key unix_time =
    let unix_time = Printf.sprintf "%.0f" unix_time in
    let command = [ "EXPIREAT"; key; unix_time ] in
    send_request connection command >>= return_bool

  (* Probably not a good idea to use this in production; see Redis documentation. *)
  let keys connection pattern =
    let command = [ "KEYS"; pattern ] in
    send_request connection command >>= return_no_nil_multibulk

  (* Move key to a different db; returns true if key was moved, false otherwise. *)
  let move connection key index =
    let index = string_of_int index in
    let command = [ "MOVE"; key; index ] in
    send_request connection command >>= return_bool

  (* Remove timeout on key; returns true if timeout was removed, false otherwise. *)
  let persist connection key =
    let command = [ "PERSIST"; key ] in
    send_request connection command >>= return_bool

  (* returns none if db is empty. *)
  let randomkey connection =
    let command = [ "randomkey" ] in
    send_request connection command >>= return_bulk

  (* Raises Error if key doesn't exist. *)
  let rename connection key newkey =
    let command = [ "RENAME"; key; newkey ] in
    send_request connection command >>= return_ok_status

  (* Raises Error if key doesn't exist; returns true if key was renamed, false if newkey already exists. *)
  let renamenx connection key newkey =
    let command = [ "RENAMENX"; key; newkey ] in
    send_request connection command >>= return_bool

  let sort
      connection
      ?by
      ?limit (* offset, limit *)
      ?get
      ?order
      ?alpha
      key =
    let command =
      sort_command
        ?by
        ?limit
        ?get
        ?order
        ?alpha
        key
    in
    send_request connection command >>= return_no_nil_multibulk

  let sort_and_store
      connection
      ?by
      ?limit (* offset, limit *)
      ?get
      ?order
      ?alpha
      key
      destination =
    let command =
      sort_command
        ?by
        ?limit
        ?get
        ?order
        ?alpha
        ~store:destination
        key
    in
    send_request connection command >>= return_int

  (* Returns None if key doesn't exist or doesn't have a timeout. *)
  let ttl connection key =
    let command = [ "TTL"; key ] in
    send_request connection command >>= return_int
      >>= function
        | -1 -> IO.return None
        | t  -> IO.return (Some t)

  (* TYPE is a reserved word in ocaml *)
  let type_of connection key =
    let command = [ "TYPE"; key ] in
    send_request connection command >>= return_status >>= function
      | `Status "string" -> IO.return `String
      | `Status "list"   -> IO.return `List
      | `Status "zset"   -> IO.return `Zset
      | `Status "hash"   -> IO.return `Hash
      | `Status "none"   -> IO.return `None (* key doesn't exist *)
      | `Status x        -> IO.fail (Unrecognized ("Unexpected TYPE result", x))
      | x                -> IO.fail (Unexpected x)

  (** String commands *)

  (* Returns length of string after append. *)
  let append connection key value =
    let command = [ "APPEND"; key; value ] in
    send_request connection command >>= return_int

  let decr connection key =
    let command = [ "DECR"; key ] in
    send_request connection command >>= return_int

  let decrby connection key decrement =
    let decrement = string_of_int decrement in
    let command = [ "DECRBY"; key; decrement ] in
    send_request connection command >>= return_int

  let get connection key =
    let command = [ "GET"; key ] in
    send_request connection command >>= return_bulk

  (* Out of range offsets will return 0. *)
  let getbit connection key offset =
    let offset = string_of_int offset in
    let command = [ "GETBIT"; key; offset ] in
   send_request connection command >>= return_int

  (* Out of range arguments are handled by limiting to valid range. *)
  let getrange connection key start stop =
    let start = string_of_int start in
    let stop = string_of_int stop in
    let command = [ "GETRANGE"; key; start; stop ] in
   send_request connection command >>= return_bulk

  (* Set value and return old value. Raises Error when key exists but isn't a string. *)
  let getset connection key value =
    let command = [ "GETSET"; key; value ] in
    send_request connection command >>= return_bulk

  let incr connection key =
    let command = [ "INCR"; key ] in
    send_request connection command >>= return_int

  let incrby connection key increment =
    let increment = string_of_int increment in
    let command = [ "INCRBY"; key; increment ] in
    send_request connection command >>= return_int

  let mget connection keys =
    let command = "MGET" :: keys in
    send_request connection command >>= return_bulk_multibulk

  (* This is atomic: either all keys are set or none are. *)
  let mset connection items =
    let command = "MSET" :: (interleave items) in
    send_request connection command >>= return_ok_status

  (* Like MSET, this is atomic. If even a single key exists, no operations will be performed.
     Returns true if all keys were set, false otherwise. *)
  let msetnx connection items =
    let command = "MSETNX" :: (interleave items) in
    send_request connection command >>= return_bool

  let set connection key value =
    let command = [ "SET"; key; value ] in
    send_request connection command >>= return_ok_status

  (* Returns the original bit value. *)
  let setbit connection key offset value =
    let offset = string_of_int offset in
    let value = string_of_int value in
    let command = [ "SETBIT"; key; offset; value ] in
    send_request connection command >>= return_int

  let setex connection key seconds value =
    let seconds = string_of_int seconds in
    let command = [ "SETEX"; key; seconds; value ] in
    send_request connection command >>= return_ok_status

  (* Returns true if key was set, false otherwise. *)
  let setnx connection key value =
    let command = [ "SETNX"; key; value ] in
    send_request connection command >>= return_bool

  (* If offset > length, string will be padded with 0-bytes. Returns length of string after modification. *)
  let setrange connection key offset value =
    let offset = string_of_int offset in
    let command = [ "SETRANGE"; key; offset; value ] in
    send_request connection command >>= return_int

  let strlen connection key =
    let command = [ "STRLEN"; key ] in
    send_request connection command >>= return_int

  (** Hash commands *)

  (* Returns true if field exists and was deleted, false otherwise. *)
  let hdel connection key field =
    let command = [ "HDEL"; key; field ] in
    send_request connection command >>= return_bool

  let hexists connection key field =
    let command = [ "HEXISTS"; key; field ] in
    send_request connection command >>= return_bool

  let hget connection key field =
    let command = [ "HGET"; key; field ] in
    send_request connection command >>= return_bulk

  let hgetall connection key =
    let command = [ "HGETALL"; key ] in
    send_request connection command >>= return_key_value_multibulk

  (* Raises error if field already contains a non-numeric value. *)
  let hincrby connection key field increment =
    let increment = string_of_int increment in
    let command = [ "HINCRBY"; key; field; increment ] in
    send_request connection command >>= return_int

  let hkeys connection key =
    let command = [ "HKEYS"; key ] in
    send_request connection command >>= return_no_nil_multibulk

  let hlen connection key =
    let command = [ "HLEN"; key ] in
    send_request connection command >>= return_int

  let hmget connection key fields =
    let command = "HMGET" :: key :: fields in
    send_request connection command >>= return_bulk_multibulk

  let hmset connection key items =
    let command = "HMSET" :: key :: (interleave items) in
    send_request connection command >>= return_ok_status

  (* Returns true if field was added, false otherwise. *)
  let hset connection key field value =
    let command = [ "HSET"; key; field; value ] in
    send_request connection command >>= return_bool

  (* Returns true if field was set, false otherwise. *)
  let hsetnx connection key field value =
    let command = [ "HSETNX"; key; field; value ] in
    send_request connection command >>= return_bool

  let hvals connection key =
    let command = [ "HVALS"; key ] in
    send_request connection command >>= return_no_nil_multibulk

  (** List commands *)

  (* Blocks while all of the lists are empty. Set timeout to number of seconds OR 0 to block indefinitely. *)
  let blpop connection keys timeout =
    let timeout = string_of_int timeout in
    let command = "BLPOP" :: (keys @ [timeout]) in
    send_request connection command >>= return_opt_pair_multibulk

  (* Same as BLPOP except pulling the last instead of first element. *)
  let brpop connection keys timeout =
    let timeout = string_of_int timeout in
    let command = "BRPOP" :: (keys @ [timeout]) in
    send_request connection command >>= return_opt_pair_multibulk

  (* Blocking RPOPLPUSH.  Returns None on timeout. *)
  let brpoplpush connection source destination timeout =
    let timeout = string_of_int timeout in
    let command = [ "BRPOPLPUSH"; source; destination; timeout ] in
    send_request connection command >>= function
      | `Multibulk []        -> IO.return None
      | `Bulk (Some element) -> IO.return (Some element)
      | x                    -> IO.fail (Unexpected x)

  (* Out of range or nonexistent key will return None. *)
  let lindex connection key index =
    let index = string_of_int index in
    let command = [ "LINDEX"; key; index ] in
    send_request connection command >>= return_bulk

  (* Returns None if pivot isn't found, otherwise returns length of list after insert. *)
  let linsert connection key where pivot value =
      let where =
          match where with
            | `Before -> "BEFORE"
            | `After -> "AFTER"
      in
      let command = [ "LINSERT"; key; where; pivot; value ] in
      send_request connection command >>= return_int
        >>= function
          | -1 -> IO.return None
          | n  -> IO.return (Some n)

  let llen connection key =
    let command = [ "LLEN"; key ] in
    send_request connection command >>= return_int

  let lpop connection key =
    let command = [ "LPOP"; key ] in
    send_request connection command >>= return_bulk

  (* Returns length of list after operation. *)
  let lpush connection key value =
    let command = [ "LPUSH"; key; value ] in
    send_request connection command >>= return_int

  (* Only push when list exists. Return length of list after operation. *)
  let lpushx connection key value =
    let command = [ "LPUSHX"; key; value ] in
    send_request connection command >>= return_int

  (* Out of range arguments are handled by limiting to valid range. *)
  let lrange connection key start stop =
    let start = string_of_int start in
    let stop = string_of_int stop in
    let command = [ "LRANGE"; key; start; stop ] in
    send_request connection command >>= return_no_nil_multibulk

  (* Returns number of elements removed. *)
  let lrem connection key count value =
    let count = string_of_int count in
    let command = [ "LREM"; key; count; value ] in
    send_request connection command >>= return_int

  (* Raises Error if out of range. *)
  let lset connection key index value =
    let index = string_of_int index in
    let command = [ "LSET"; key; index; value ] in
    send_request connection command >>= return_ok_status

  (* Removes all but the specified range. Out of range arguments are handled by limiting to valid range. *)
  let ltrim connection key start stop =
    let start = string_of_int start in
    let stop = string_of_int stop in
    let command = [ "LTRIM"; key; start; stop ] in
    send_request connection command >>= return_ok_status

  let rpop connection key =
    let command = [ "RPOP"; key ] in
    send_request connection command >>= return_bulk

  (* Remove last element of source and insert as first element of destination. Returns the element moved
     or None if source is empty. *)
  let rpoplpush connection source destination =
    let command = [ "RPOPLPUSH"; source; destination ] in
    send_request connection command >>= return_bulk

  (* Returns length of list after operation. *)
  let rpush connection key value =
    let command = [ "RPUSH"; key; value ] in
    send_request connection command >>= return_int

  let rpushx connection key value =
    let command = [ "RPUSHX"; key; value ] in
    send_request connection command >>= return_int

  (** Set commands *)

  (* Returns true if member was added, false otherwise. *)
  let sadd connection key member =
    let command = [ "SADD"; key; member ] in
    send_request connection command >>= return_bool

  let scard connection key =
    let command = [ "SCARD"; key ] in
    send_request connection command >>= return_int

  (* Difference between first and all successive sets. *)
  let sdiff connection keys =
    let command = "SDIFF" :: keys in
    send_request connection command >>= return_no_nil_multibulk

  (* like sdiff, but store result in destination. returns size of result. *)
  let sdiffstore connection destination keys =
    let command = "sdiffstore" :: destination :: keys in
    send_request connection command >>= return_int

  let sinter connection keys =
    let command = "SINTER" :: keys in
    send_request connection command >>= return_no_nil_multibulk

  (* Like SINTER, but store result in destination. Returns size of result. *)
  let sinterstore connection destination keys =
    let command = "SINTERSTORE" :: destination :: keys in
    send_request connection command >>= return_int

  let sismember connection key member =
    let command = [ "SISMEMBER"; key; member ] in
    send_request connection command >>= return_bool

  let smembers connection key =
    let command = [ "SMEMBERS"; key ] in
    send_request connection command >>= return_no_nil_multibulk

  (* Returns true if an element was moved, false otherwise. *)
  let smove connection source destination member =
    let command = [ "SMOVE"; source; destination; member ] in
    send_request connection command >>= return_bool

  (* Remove random element from set. *)
  let spop connection key =
    let command = [ "SPOP"; key ] in
    send_request connection command >>= return_bulk

  (* Like SPOP, but doesn't remove chosen element. *)
  let srandmember connection key =
    let command = [ "SRANDMEMBER"; key ] in
    send_request connection command >>= return_bulk

  (* Returns true if element was removed. *)
  let srem connection key member =
    let command = [ "SREM"; key; member ] in
    send_request connection command >>= return_bool

  let sunion connection keys =
    let command = "SUNION" :: keys in
    send_request connection command >>= return_no_nil_multibulk

  (* Like SUNION, but store result in destination. Returns size of result. *)
  let sunionstore connection destination keys =
    let command = "SUNIONSTORE" :: destination :: keys in
    send_request connection command >>= return_int


  (** Sorted set commands *)


  (** Pub/sub commands *)

  (* Post a message to a channel. Returns number of clients that received the message. *)
  let publish connection channel message =
    let command = [ "PUBLISH" ; channel ; message ] in
    send_request connection command >>= return_int

  (* Lists the currently active channels. If no pattern is specified, all channels are listed. *)
  let pubsub_channels connection channels =
    let message = Option.default "*" channels in
    let command = ["PUBSUB"; "CHANNELS"; message ] in
    send_request connection command >>= return_multibulk

  (** Transaction commands *)

  (* Marks the start of a transaction block. Subsequent commands will be queued for atomic execution using EXEC. *)
  let multi connection =
    let command = [ "MULTI" ] in
    send_request connection command >>= return_ok_status

  (* Executes all previously queued commands in a transaction and restores the connection state to normal. *)
  let exec connection =
    let command = [ "EXEC" ] in
    send_request connection command >>= return_multibulk

  (* Flushes all previously queued commands in a transaction and restores the connection state to normal. *)
  let discard connection =
    let command = [ "DISCARD" ] in
    send_request connection command >>= return_ok_status

  (* Marks the given keys to be watched for conditional execution of a transaction. *)
  let watch connection keys =
    let command = "WATCH" :: keys in
    send_request connection command >>= return_ok_status

  (* Flushes all the previously watched keys for a transaction. *)
  let unwatch connection =
    let command = [ "UNWATCH" ] in
    send_request connection command >>= return_ok_status

  let queue fn =
    IO.try_bind fn (fun _ -> IO.return ())
      (function
        | Unexpected x -> return_queued_status x
        | e -> IO.fail e)

  (** Server *)

  let bgrewriteaof connection =
    let command = [ "BGREWRITEAOF" ] in
    send_request connection command >>= return_ok_status

  let bgsave connection =
    let command = [ "BGSAVE" ] in
    send_request connection command >>= return_ok_status

  let config_resetstat connection =
    let command = [ "CONFIG"; "RESETSTAT" ] in
    send_request connection command >>= return_ok_status

  let dbsize connection =
    let command = [ "DBSIZE" ] in
    send_request connection command >>= return_int

  (* clear all databases *)
  let flushall connection =
    let command = [ "FLUSHALL" ] in
    send_request connection command >>= return_ok_status

  (* clear current database *)
  let flushdb connection =
    let command = [ "FLUSHDB" ] in
    send_request connection command >>= return_ok_status

  let info connection =
    let command = [ "INFO" ] in
    send_request connection command >>= return_info_bulk

  (* last successful save as Unix timestamp *)
  let lastsave connection =
    let command = [ "LASTSAVE" ] in
    send_request connection command >>= return_float

  (* synchronous save *)
  let save connection =
    let command = [ "SAVE" ] in
    send_request connection command >>= return_ok_status

  (* save and shutdown server *)
  let shutdown connection =
    let command = [ "SHUTDOWN" ] in
    IO.try_bind
      (fun () -> send_request connection command)
      (fun (_ : reply) -> IO.return ())
      (function
        | End_of_file -> IO.return ()
        | e           -> IO.fail e)
end
