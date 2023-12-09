type t = int * int [@@deriving show]

type intersect =
  | Subset of int * int
  | Superset of int * int
  | Overlap of int * int

let intersect (a, b) = function
  | x, y when a <= x && y <= b -> Some (Subset (x, y))
  | x, y when x <= a && b <= y -> Some (Superset (x, y))
  | x, y when x <= b && b <= y -> Some (Overlap (x, b))
  | x, y when x <= a && a <= y -> Some (Overlap (a, y))
  | _ -> None

let add n (a, b) = (a + n, b + n)

let to_list (start, stop) =
  let rec aux acc current =
    if current < start then
      acc
    else
      aux (current :: acc) (current - 1)
  in
  aux [] stop
