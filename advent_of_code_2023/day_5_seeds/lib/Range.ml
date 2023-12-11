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

let union (Range (a, b) as r1) (Range (x, y) as r2) =
  match intersect r1 r2 with
  | Some (Subset _) -> Some (Range (x, y))
  | Some (Superset _) -> Some (Range (a, b))
  | Some (OverlapRight _) -> Some (Range (a, y))
  | Some (OverlapLeft _) -> Some (Range (x, b))
  | None -> None

let union_list ranges =
  let rec aux acc = function
    | [] -> acc
    | r :: rs ->
    match acc with
    | [] -> aux [ r ] rs
    | r' :: rs' ->
    match union r r' with
    | Some r'' -> aux (r'' :: rs') rs
    | None -> aux (r :: acc) rs
  in
  aux [] ranges
