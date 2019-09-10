module Make(IO : S.IO)(Client : S.Client with module IO = IO)(Params : S.Cache_params) = struct
  module IO = IO
  module Client = Client
  module Params = Params

  let (>>=) = IO.(>>=)

  let set r key data =
    let key = Params.cache_key key in
    let data = Params.string_of_data data in
    Client.set r key data >>= fun _res ->
    match Params.cache_expiration with 
    | None -> IO.return () 
    | Some cache_expiration -> 
        Client.expire r key cache_expiration >>= (fun _ -> IO.return ())

  let get r key =
    let key = Params.cache_key key in
    Client.get r key >>= fun value ->
    IO.return (Utils.Option.map Params.data_of_string value)

  let delete r key =
    let key = Params.cache_key key in
    IO.ignore_result (Client.del r [key])
end
