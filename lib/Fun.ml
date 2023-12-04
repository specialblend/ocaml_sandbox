let ( >> ) f g x = g (f x)
let is_between (min, max) x = min <= x && x <= max
let len_of_int = string_of_int >> String.length
let nth n l = List.nth l n

module List = struct
  module L = List
  include List

  let sum = fold_left ( + ) 0
  let product = fold_left ( * ) 1
  let nth n l = nth l n
end
