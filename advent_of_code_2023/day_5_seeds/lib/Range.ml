open Fun

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

let ( ^ ) = intersect

let union ((a, b) as r1) ((x, y) as r2) =
  match intersect r1 r2 with
  | Some (Subset _) -> Some (a, b)
  | Some (Superset _) -> Some (x, y)
  | Some (OverlapRight _) -> Some (a, y)
  | Some (OverlapLeft _) -> Some (x, b)
  | None -> None

let ( ++ ) = union

let union_all rgs =
  let rec union_all = function
    | r1 :: r2 :: rest -> begin
        match union r1 r2 with
        | Some r -> union_all (r :: rest)
        | None -> r1 :: union_all (r2 :: rest)
      end
    | rest -> rest
  in
  union_all rgs

(*
   type diff =
     | Empty
     | Unit of t
     | Pair of t * t
   [@@deriving show]
*)

let diff (a, b) (x, y) =
  match intersect (a, b) (x, y) with
  | _ when a = x && b = y -> []
  | Some (Superset _) -> []
  | Some (Subset (x, y)) when x = a -> [ (y + 1, b) ]
  | Some (Subset (x, y)) when y = b -> [ (a, x - 1) ]
  | Some (Subset (x, y)) -> [ (a, x - 1); (y + 1, b) ]
  | Some (OverlapRight (x, _)) -> [ (a, x - 1) ]
  | Some (OverlapLeft (_, y)) -> [ (y + 1, b) ]
  | None -> [ (a, b) ]

(*
   let diff2 (a, b) (x, y) =
     match intersect (a, b) (x, y) with
     | _ when a = x && b = y -> Empty
     | Some (Superset _) -> Empty
     | Some (Subset (x, y)) when x = a -> Unit (y + 1, b)
     | Some (Subset (x, y)) when y = b -> Unit (a, x - 1)
     | Some (Subset (x, y)) -> Pair ((a, x - 1), (y + 1, b))
     | Some (OverlapRight (x, _)) -> Unit (a, x - 1)
     | Some (OverlapLeft (_, y)) -> Unit (y + 1, b)
     | None -> Unit (a, b)      
*)

let diff_all r ranges =
  let rec diff_all r1 rest =
    rest
    |>| union_all
    |>| List.sort compare
    |>| function
    | [] -> r1 :: []
    | r2 :: rest ->
    match diff r1 r2 with
    | [] -> []
    | r3 :: [] -> diff_all r3 rest
    | r3 :: r4 :: _ -> r3 :: diff_all r4 rest
  in
  diff_all r ranges
