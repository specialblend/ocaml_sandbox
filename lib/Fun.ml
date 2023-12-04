let ( >> ) f g x = g (f x)
let is_between (min, max) x = min <= x && x <= max
let len_of_int = string_of_int >> String.length

module List = struct
  include List

  let sum = fold_left ( + ) 0
  let product = fold_left ( * ) 1
end
