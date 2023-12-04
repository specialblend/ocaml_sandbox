let ( >> ) f g x = g (f x)
let is_between (min, max) x = min <= x && x <= max
let is_numeric e = int_of_string_opt e <> None

module List = struct
  module L = List
  include List

  let sum = fold_left ( + ) 0
  let product = fold_left ( * ) 1
  let nth n l = nth l n
end
