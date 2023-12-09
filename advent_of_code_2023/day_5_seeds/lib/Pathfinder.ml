open Fun
open Contract

type t = {
  window: Range.t;
  offset: int;
}
[@@deriving show]

let range_of (_, src, margin) = (src, src + margin - 1)

let row_intersects range row =
  let r = range_of row in
  match Range.intersect r range with
  | Some (Subset (_, _)) -> true
  | Some (Overlap (_, _)) -> true
  | _ -> false

let rec fold_table result = function
  | rows :: table -> (
      match result with
      | None -> None
      | Some result -> (
          let path, range = result in
          match rows |> List.find_opt (row_intersects range) with
          | None -> None
          | Some row ->
          match Range.intersect (range_of row) range with
          | Some (Subset (_x, _y)) ->
              let dst, src, _ = row in
              let offset' = dst - src in
              let path' = { path with offset = path.offset + offset' } in
              let range' = Range.add offset' range in
              let result' = (path', range') in
              fold_table (Some result') table
          | Some (Overlap (x, y)) ->
              let a, b = range in
              let d_left = x - a in
              let d_right = y - b in
              let a', b' = path.window in
              let window' = (a' + d_left, b' + d_right) in
              let dst, src, _ = row in
              let offset' = dst - src in
              let path' =
                { window = window'; offset = path.offset + offset' }
              in
              let range' = Range.add offset' (x, y) in
              let result' = (path', range') in
              fold_table (Some result') table
          | _ -> None))
  | _ -> result

let compile_header table header =
  let dst, src, margin = header in
  let window = (src, src + margin - 1) in
  let offset = dst - src in
  let range = Range.add offset window in
  let path = { window; offset } in
  let init = (path, range) in
  match fold_table (Some init) table with
  | Some (path, _) -> Some path
  | None -> None

let compile_table : table -> t list = function
  | init :: table -> List.filter_map (compile_header table) init
  | _ -> failwith "illegal"
