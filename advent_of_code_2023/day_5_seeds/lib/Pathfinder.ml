open Fun

module Path = struct
  type t = {
    window: Range.t;
    offset: int;
  }
  [@@deriving show]

  type cursor = t * Range.t

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
end

let range_of (_, src, margin) = (src, src + margin - 1)

let row_intersects range row =
  let r = range_of row in
  match Range.intersect r range with
  | Some (Subset (_, _)) -> true
  | Some (Overlap (_, _)) -> true
  | _ -> false

let fold_table init_result init_table =
  let rec fold (result, table) =
    let scan_rows prev rows table_rest =
      let _, range = prev in
      let match_row row =
        let a, b = range_of row in
        let next_result =
          match Range.intersect (a, b) range with
          | Some (Subset (x, y)) -> Path.use_subset prev row (x, y)
          | Some (Overlap (x, y)) -> Path.use_overlap prev row (x, y)
          | _ -> None
        in
        fold (next_result, table_rest)
      in
      match List.find_opt (row_intersects range) rows with
      | Some row -> match_row row
      | None -> None
    in
    match (result, table) with
    | Some prev, rows :: table_rest -> scan_rows prev rows table_rest
    | _ -> result
  in
  fold (init_result, init_table)

let compile_header header =
  let dst, src, margin = header in
  let window = (src, src + margin - 1) in
  let offset = dst - src in
  let range = Range.add offset window in
  let path = Path.{ window; offset } in
  let init_result = (path, range) in
  init_result

let compile_table =
  let use_header table header =
    let init_result = compile_header header in
    match fold_table (Some init_result) table with
    | Some (path, _) -> Some path
    | None -> None
  in
  let is_singleton = function
    | Path.{ window = a, b; _ } -> a = b
  in
  let fix_window path =
    (* idk why but left needs incremented by 1 *)
    let Path.{ window = left, right; _ } = path in
    Path.{ path with window = (left + 1, right) }
  in
  function
  | headers :: table ->
      headers
      |>| List.filter_map (use_header table)
      |>| List.filter (fun path -> not (is_singleton path))
      |>| List.map fix_window
  | _ -> failwith "illegal"
