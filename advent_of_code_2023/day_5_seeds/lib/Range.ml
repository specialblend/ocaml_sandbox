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
  | x, y when a <= x && y <= b -> Some (x, y)
  | x, y when x <= a && b <= y -> Some (a, b)
  | x, y when x <= b && b <= y -> Some (x, b)
  | x, y when x <= a && a <= y -> Some (a, y)
  | _ -> None

let intersect2 (a, b) = function
  | x, y when a <= x && y <= b -> Some (Subset (x, y))
  | x, y when x <= a && b <= y -> Some (Superset (x, y))
  | x, y when x <= b && b <= y -> Some (OverlapRight (x, b))
  | x, y when x <= a && a <= y -> Some (OverlapLeft (a, y))
  | _ -> None

let union ((a, b) as r1) ((x, y) as r2) =
  match intersect r1 r2 with
  | Some _ -> Some (min a x, max b y)
  | None -> None

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

let diff r1 r2 =
  let a, b = r1
  and x, y = r2 in
  match intersect r1 r2 with
  | _ when r1 = r2 -> []
  | Some superset when superset = r1 -> []
  | Some (x, y) when x = a -> [ (y + 1, b) ]
  | Some (x, y) when y = b -> [ (a, x - 1) ]
  | Some subset when subset = r2 -> [ (a, x - 1); (y + 1, b) ]
  | Some (x, _) when x <= b -> [ (a, x - 1) ]
  | Some (_, y) -> [ (y + 1, b) ]
  | None -> [ r1 ]

let diff_all r ranges =
  let rec diff_all r1 = function
    | [] -> r1 :: []
    | r2 :: rest ->
    match diff r1 r2 with
    | [] -> []
    | r3 :: [] -> diff_all r3 rest
    | r3 :: r4 :: _ -> r3 :: diff_all r4 rest
  in
  diff_all r (ranges |> union_all |> List.sort compare)
