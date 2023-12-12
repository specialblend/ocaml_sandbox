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

let union_list rgs =
  let rec aux = function
    | r1 :: r2 :: rest -> begin
        match union r1 r2 with
        | Some r -> aux (r :: rest)
        | None -> r1 :: aux (r2 :: rest)
      end
    | rest -> rest
  in
  aux rgs

let diff (a, b) (x, y) =
  match intersect (a, b) (x, y) with
  | Some (Subset (x, y)) -> [ (a, x - 1); (y + 1, b) ]
  | Some (Superset (x, y)) -> [ (x, a - 1); (b + 1, y) ]
  | Some (OverlapRight (x, _)) -> [ (a, x - 1) ]
  | Some (OverlapLeft (_, y)) -> [ (y + 1, b) ]
  | None -> [ (a, b) ]

let diff_list (r : t) rgs =
  let rec aux acc rest =
    match rest with
    | [] -> acc
    | r2 :: rest ->
        let acc' = List.concat_map (diff r2) acc in
        aux acc' rest
  in
  aux [ r ] rgs
