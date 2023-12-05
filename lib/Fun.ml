let ( >> ) f g x = g (f x)
let is_between (min, max) x = min <= x && x <= max
let is_numeric e = int_of_string_opt e <> None
let pair x y = (x, y)

module List = struct
  include List

  let sum = fold_left ( + ) 0
  let product = fold_left ( * ) 1
  let nth n l = nth l n
end

module Str = struct
  include Str

  let split_by r s = Str.split (Str.regexp r) s
end
