(**
   Redis client
 *)
module Make(IO : S.IO) : S.Client with module IO = IO
