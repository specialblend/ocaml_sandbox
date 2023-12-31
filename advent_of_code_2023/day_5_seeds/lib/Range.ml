type t = int * int [@@deriving show { with_path = false }]

type intersect =
  | Subset of int * int
  | Superset of int * int
  | OverlapRight of int * int
  | OverlapLeft of int * int
[@@deriving show { with_path = false }]

let intersect (a, b) = function
  | x, y when a <= x && y <= b -> Some (Subset (x, y))
  | x, y when x <= a && b <= y -> Some (Superset (x, y))
  | x, y when x <= b && b <= y -> Some (OverlapRight (x, b))
  | x, y when x <= a && a <= y -> Some (OverlapLeft (a, y))
  | _ -> None

let add_both n (a, b) = (a + n, b + n)

let to_list (start, stop) =
  let rec aux acc current =
    if current < start then
      acc
    else
      aux (current :: acc) (current - 1)
  in
  aux [] stop
