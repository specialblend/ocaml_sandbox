open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type seed = int * int [@@deriving show]
type row = int * int * int [@@deriving show]
type map = row list [@@deriving show]
type table = map list [@@deriving show]
type almanac = seed list * table [@@deriving show]

let rec chunk_pairs = function
  | x :: y :: rest -> (x, y) :: chunk_pairs rest
  | _ -> []

let parse_seeds =
  List.concat_map (List.filter_map int_of_string_opt) >> chunk_pairs

let parse_triple = function
  | [ dst; src; range ] -> Some (dst, src, range)
  | _ -> None

let parse_map = List.filter_map int_of_string_opt >> parse_triple
let parse_maps = List.map (List.filter_map parse_map)

let parse_almanac text : almanac =
  let parse_group text =
    text
    |>| Str.split _EOL
    |>| List.map (Regex.find_all _NUM >> List.rev)
    |>| List.filter (fun x -> List.length x > 0)
  in
  Str.split _EOL2 text
  |>| List.map parse_group
  |>| function
  | h :: t -> (parse_seeds h, parse_maps t)
  | _ -> failwith "illegal"

module Range = struct
  type t = int * int

  type intersect =
    | Subset of int * int
    | Superset of int * int
    | Overlap of int * int

  let intersect (a, b) = function
    | x, y when a <= x && y <= b -> Some (Subset (x, y))
    | x, y when x <= a && b <= y -> Some (Superset (x, y))
    | x, y when x <= a && a <= y -> Some (Overlap (a, y))
    | x, y when x <= b && b <= y -> Some (Overlap (x, b))
    | _ -> None

  let add n (a, b) = (a + n, b + n)
end

type path = {
  window: Range.t;
  offset: int;
}

let range_of (_, src, margin) = (src, src + margin - 1)

let row_intersects range row =
  match Range.intersect range (range_of row) with
  | Some (Subset (_, _)) -> true
  | Some (Overlap (_, _)) -> true
  | _ -> false

let compile_rows result rows =
  let path, range = result in
  match rows |> List.find_opt (row_intersects range) with
  | None -> result
  | Some row ->
  match Range.intersect range (range_of row) with
  | Some (Subset _) ->
      let dst, src, _ = row in
      let offset = dst - src in
      let path = { path with offset = path.offset + offset } in
      (path, Range.add offset range)
  | Some (Overlap (x, y)) ->
      let a, b = range in
      let d_left = x - a in
      let d_right = b - y in
      let a', b' = path.window in
      let window = (a' + d_left, b' - d_right) in
      let dst, src, _ = row in
      let offset = dst - src in
      let path = { window; offset } in
      let range = (x, y) in
      (path, Range.add offset range)
  | _ -> result

let compile_header_row table row =
  let dst, src, margin = row in
  let window = (src, src + margin - 1) in
  let offset = dst - src in
  let path = { window; offset } in
  let init = (path, window) in
  let path, _ = List.fold_left compile_rows init table in
  path

let compile_table : table -> path list = function
  | init :: table -> List.map (compile_header_row table) init
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
