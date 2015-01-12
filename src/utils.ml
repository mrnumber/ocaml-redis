module List = struct
  let filter_map f l =
    let rec loop l accum =
      match l with
      | [] -> accum
      | hd :: tl ->
         match f hd with
         | Some x -> loop tl (x :: accum)
         | None -> loop tl accum
    in
    List.rev (loop l [])
end

module String = struct
  module Str = Re_str

  let nsplit str delim = Str.split (Str.regexp delim) str

  let split str delim =
    match Str.bounded_split (Str.regexp delim) str 2 with
    | a :: b :: [] -> Some (a, b)
    | _ -> None
end

module Option = struct
  let default d = function
    | None -> d
    | Some v -> v

  let may f = function
    | None -> ()
    | Some v -> f v; ()

  let map f = function
    | None -> None
    | Some v -> Some (f v)
end
