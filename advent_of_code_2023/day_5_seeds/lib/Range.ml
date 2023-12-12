type t = int * int [@@deriving show { with_path = false }]

let map fn (a, b) = (fn a, fn b)

type intersect =
  | Subset of t
  | Superset of t
  | OverlapRight of t
  | OverlapLeft of t
[@@deriving show { with_path = false }]

let intersect (a, b) = function
  | x, y when a <= x && y <= b -> Some (Subset (x, y))
  | x, y when x <= a && b <= y -> Some (Superset (x, y))
  | x, y when x <= b && b <= y -> Some (OverlapRight (x, b))
  | x, y when x <= a && a <= y -> Some (OverlapLeft (a, y))
  | _ -> None

let union ((a, b) as r1) ((x, y) as r2) =
  match intersect r1 r2 with
  | Some (Subset _) -> Some (x, y)
  | Some (Superset _) -> Some (a, b)
  | Some (OverlapRight _) -> Some (a, y)
  | Some (OverlapLeft _) -> Some (x, b)
  | None -> None

let union_list s =
  let rec join = function
    | r1 :: r2 :: rest -> begin
        match union r1 r2 with
        | Some r -> join (r :: rest)
        | None -> r1 :: join (r2 :: rest)
      end
    | rest -> rest
  in
  join s

let diff (a, b) (x, y) =
  match intersect (a, b) (x, y) with
  | Some (Subset (x, y)) -> [ (a, x - 1); (y + 1, b) ]
  | Some (Superset (x, y)) -> [ (x, a - 1); (b + 1, y) ]
  | Some (OverlapRight (x, _)) -> [ (a, x - 1) ]
  | Some (OverlapLeft (_, y)) -> [ (y + 1, b) ]
  | None -> [ (a, b) ]
