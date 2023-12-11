open Fun

module Path = struct
  type t = {
    domain: Range.t;
    offset: int;
  }
  [@@deriving show { with_path = false }]
end

module Cursor = struct
  type t = {
    path: Path.t;
    range: Range.t;
  }
  [@@deriving show { with_path = false }]
end

let range_of (_, src, margin) = (src, src + margin - 1)

let row_intersects range row =
  let r = range_of row in
  match Range.intersect r range with
  | Some (Subset (_, _)) -> true
  | Some (OverlapRight (_, _)) -> true
  | Some (OverlapLeft (_, _)) -> true
  | _ -> false

let use_subset _ cursor row =
  let Cursor.{ path; range } = cursor in
  let dst, src, _ = row in
  let offset = dst - src in
  let path = { path with offset = path.offset + offset } in
  let range = Range.add_both offset range in
  let cursor = Cursor.{ path; range } in
  cursor

let use_overlap (x, y) cursor row =
  let Cursor.{ path; range } = cursor in
  let a, b = range in
  let d_left = x - a in
  let d_right = y - b in
  let left, right = path.domain in
  let domain' = (left + d_left, right + d_right) in
  let dst, src, _ = row in
  let offset = dst - src in
  let path = { Path.domain = domain'; offset = path.offset + offset } in
  let range = Range.add_both offset (x, y) in
  let cursor = Cursor.{ path; range } in
  cursor

let move_cursor fold prev rest row =
  let Cursor.{ range; _ } = prev in
  let next_range = range_of row in
  let next_cursor =
    let next_move =
      match Range.intersect next_range range with
      | Some (Subset (x, y)) -> Some (use_subset (x, y))
      | Some (OverlapLeft (x, y)) -> Some (use_overlap (x, y))
      | Some (OverlapRight (x, y)) -> Some (use_overlap (x, y))
      | _ -> None
    in
    match next_move with
    | Some move -> Some (move prev row)
    | None -> None
  in
  fold next_cursor rest

let fold_table cursor table =
  let rec fold cursor table =
    let scan_rows prev rows rest =
      let Cursor.{ range; _ } = prev in
      let move_cursor = move_cursor fold prev rest in
      match List.find_opt (row_intersects range) rows with
      | Some row -> move_cursor row
      | None -> None
    in
    match (cursor, table) with
    | Some prev, rows :: table -> scan_rows prev rows table
    | _ -> cursor
  in
  fold cursor table

let compile_header header =
  let dst, src, margin = header in
  let domain = (src, src + margin - 1) in
  let offset = dst - src in
  let range = Range.add_both offset domain in
  let path = Path.{ domain; offset } in
  let cursor = Cursor.{ path; range } in
  cursor

let is_singleton = function
  | Path.{ domain = a, b; _ } -> a = b

let fix_window path =
  (* idk why but left needs incremented by 1 *)
  let Path.{ domain = left, right; _ } = path in
  Path.{ path with domain = (left + 1, right) }

let compile_paths table =
  let scan_header table header =
    let cursor = compile_header header in
    match fold_table (Some cursor) table with
    | Some { path; _ } -> Some path
    | None -> None
  in
  match table with
  | headers :: table ->
      headers
      |>| List.filter_map (scan_header table)
      |>| List.filter (fun path -> not (is_singleton path))
      |>| List.map fix_window
      |>| List.sort compare
  | _ -> failwith "illegal"

type known_seed = (int * int) * int option [@@deriving show]
type known_seeds = known_seed list list [@@deriving show]

let compile_path seed path =
  let Path.{ domain; offset } = path in
  let start, margin = seed in
  let a, b = domain in
  let range = (start, start + margin) in
  let _x, _y = range in
  match Range.intersect domain range with
  | None -> None
  | Some (Subset _) -> Some [ (range, Some offset) ]
  | Some (OverlapRight (x', y')) ->
      let left = (a, x' - 1) in
      let right = (x', y') in
      Some
        [
          (left, None);
          (right, Some offset);
          (* ------------------------------- *)
        ]
  | Some (OverlapLeft (x', y')) ->
      let left = (x', y') in
      let right = (y' + 1, b) in
      Some
        [
          (left, Some offset);
          (right, None);
          (* ------------------------------- *)
        ]
  | Some (Superset (x, y)) ->
      let left = (x, a - 1) in
      let right = (b + 1, y) in
      let middle = (a, b) in
      Some
        [
          (left, None);
          (middle, Some offset);
          (right, None);
          (* --------------  *)
        ]

let merge_unknown_seeds (seeds : known_seeds) =
  let check_dupe seed seed' =
    match seed' with
    | _, Some _ -> true
    | range', None -> begin
        let range, _ = seed in
        match Range.intersect range range' with
        | Some (Subset _) -> false
        | _ -> true
      end
  in
  let seeds' = seeds |>| List.flatten |>| List.sort compare in
  seeds'
  |>| List.fold_left
        (fun acc seed ->
          let res = List.filter (check_dupe seed) acc in
          seed :: res)
        []

type compiled_seeds = known_seed list [@@deriving show]

let compile_known_seeds table seeds =
  let compile seed =
    table
    |>| compile_paths
    |>| List.filter_map (fun path -> compile_path seed path)
  in
  List.concat_map compile seeds |>| merge_unknown_seeds
