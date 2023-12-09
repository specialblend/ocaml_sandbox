open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type seed = int * int [@@deriving show]
type col = int * int * int [@@deriving show]
type seeds = seed list [@@deriving show]
type row = col list [@@deriving show]
type table = row list [@@deriving show]
type map = seed list * table [@@deriving show]

let rec chunk_pairs = function
  | x :: y :: rest -> (x, y) :: chunk_pairs rest
  | _ -> []

let parse_seeds =
  List.concat_map (List.filter_map int_of_string_opt) >> chunk_pairs

let parse_triple = function
  | [ dst; src; range ] -> Some (dst, src, range)
  | _ -> None

let parse_mapping = List.filter_map int_of_string_opt >> parse_triple
let parse_mappings = List.map (List.filter_map parse_mapping)

(*
   (a, b) -> (x, y)
   def is_range_inside(subset, superset):
    a_start, a_end = subset
    b_start, b_end = superset
    return b_start <= a_start and a_end <= b_end
*)

module Range = struct
  type t = int * int

  type intersect =
    | Subset of int * int
    | Superset of int * int
    | Overlap of int * int

  let intersect (a, b) = function
    | x, y when x <= a && b <= y -> Some (Superset (a, b))
    | x, y when a <= x && y <= b -> Some (Subset (x, y))
    | x, y when x <= a && a <= y -> Some (Overlap (a, y))
    | x, y when x <= b && b <= y -> Some (Overlap (x, b))
    | _ -> None

  let add n (a, b) = (a + n, b + n)
end

type path = {
  window: Range.t;
  offset: int;
}

let intersects left right =
  match Range.intersect left right with
  | Some (Subset (_, _)) -> true
  | Some (Overlap (_, _)) -> true
  | _ -> false

let range_of (_, src, margin) = (src, src + margin - 1)

let scan_row acc row =
  let path, range = acc in
  match row |> List.find_opt (fun col -> range_of col |> intersects range) with
  | None -> acc
  | Some col ->
  match Range.intersect range (range_of col) with
  | Some (Subset _) ->
      let dst, src, _ = col in
      let offset' = dst - src in
      let range = Range.add offset' range in
      let offset = path.offset + offset' in
      ({ path with offset }, range)
  | Some (Overlap (_, src)) -> acc
  | _ -> acc

let find_path rows (dst, src, margin) =
  let window = (src, src + margin - 1) in
  let offset = dst - src in
  let path = { window; offset } in
  let init = (path, window) in
  let path, _ = List.fold_left scan_row init rows in
  path

let compile_table : table -> path list = function
  | init :: rows -> List.map (find_path rows) init
  | _ -> failwith "illegal"

let look_row n row =
  let look n col =
    match col with
    | dst, src, range when is_between (src, src + range) n ->
        Some (dst + n - src)
    | _ -> None
  in
  let r =
    List.fold_left
      (fun acc x ->
        match acc with
        | Some x -> Some x
        | None -> look n x)
      None row
  in
  match r with
  | Some x -> x
  | None -> n

let look_table = List.fold_left look_row

let look_min (seeds, table) =
  List.map (fun seed -> look_table seed table) seeds
  |> List.fold_left min max_int

let parse_sect sect =
  sect
  |>| Str.split _EOL
  |>| List.map (Regex.find_all _NUM >> List.rev)
  |>| List.filter (fun x -> List.length x > 0)

let parse_all text : map =
  Str.split _EOL2 text
  |>| List.map parse_sect
  |>| function
  | h :: t -> (parse_seeds h, parse_mappings t)
  | _ -> failwith "illegal"
