let ( >> ) f g x = g (f x)
let is_between (min, max) x = min <= x && x <= max
