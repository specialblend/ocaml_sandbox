type t = Range of int * int [@@deriving show { with_path = false }]

let make (a, b) = Range (a, b)

type intersect =
  | Subset of t
  | Superset of t
  | OverlapRight of t
  | OverlapLeft of t
[@@deriving show { with_path = false }]

let intersect (Range (a, b)) = function
  | Range (x, y) when a <= x && y <= b -> Some (Subset (Range (x, y)))
  | Range (x, y) when x <= a && b <= y -> Some (Superset (Range (x, y)))
  | Range (x, y) when x <= b && b <= y -> Some (OverlapRight (Range (x, b)))
  | Range (x, y) when x <= a && a <= y -> Some (OverlapLeft (Range (a, y)))
  | _ -> None
