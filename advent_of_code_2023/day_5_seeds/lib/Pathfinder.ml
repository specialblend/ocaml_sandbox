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

let use_overlap (path, range) row (x, y) =
  let a, b = range in
  let d_left = x - a in
  let d_right = y - b in
  let left, right = path.window in
  let window' = (left + d_left, right + d_right) in
  let dst, src, _ = row in
  let offset = dst - src in
  let path = { window = window'; offset = path.offset + offset } in
  let range = Range.add offset (x, y) in
  let result = (path, range) in
  Some result

let use_subset (path, range) row _ =
  let dst, src, _ = row in
  let offset = dst - src in
  let path = { path with offset = path.offset + offset } in
  let range = Range.add offset range in
  let result = (path, range) in
  Some result

let fold_table init_result init_table =
  let rec fold (result, table) =
    match (result, table) with
    | Some prev_result, rows :: table_rest -> begin
        let _, range = prev_result in
        match List.find_opt (row_intersects range) rows with
        | Some row -> begin
            let a, b = range_of row in
            let next_result =
              match Range.intersect (a, b) range with
              | Some (Subset (x, y)) -> use_subset prev_result row (x, y)
              | Some (Overlap (x, y)) -> use_overlap prev_result row (x, y)
              | _ -> None
            in
            fold (next_result, table_rest)
          end
        | None -> None
      end
    | _ -> result
  in
  fold (init_result, init_table)

let compile_header : int * int * int -> t * (int * int) =
 fun header ->
  let dst, src, margin = header in
  let window = (src, src + margin - 1) in
  let offset = dst - src in
  let range = Range.add offset window in
  let path = { window; offset } in
  let init_result = (path, range) in
  init_result

let compile_table : table -> t list =
  let use_header table header =
    let init_result = compile_header header in
    match fold_table (Some init_result) table with
    | Some (path, _) -> Some path
    | None -> None
  in
  let is_singleton = function
    | { window = a, b; _ } -> a = b
  in
  let fix_window path =
    (* idk why but left needs incremented by 1 *)
    let left, right = path.window in
    let window = (succ left, right) in
    { path with window }
  in
  function
  | headers :: table ->
      headers
      |>| List.filter_map (use_header table)
      |>| List.filter (fun path -> not (is_singleton path))
      |>| List.map fix_window
  | _ -> failwith "illegal"
