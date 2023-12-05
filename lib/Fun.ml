let ( >> ) f g x = g (f x)
let ( ->. ) = ( >> )
let ( ->| ) = ( |> )
let is_between (min, max) x = min <= x && x <= max
let is_numeric e = int_of_string_opt e <> None
let pair x y = (x, y)

module Array = struct
  include Array

  let sum = fold_left ( + ) 0
  let product = fold_left ( * ) 1
end

module List = struct
  include List

  let fill x = map (fun _ -> x)
  let sum = fold_left ( + ) 0
  let product = fold_left ( * ) 1
  let nth n l = nth l n

  let update_range fn (i, i') =
    mapi (fun k v -> if k |> is_between (i, i') then fn v else v)
end

module Str = struct
  include Str

  let split_by r s = split (Str.regexp r) s
end
