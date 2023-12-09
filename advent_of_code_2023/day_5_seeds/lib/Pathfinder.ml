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
  let a', b' = path.window in
  let window' = (a' + d_left, b' + d_right) in
  let dst, src, _ = row in
  let offset' = dst - src in
  let path' = { window = window'; offset = path.offset + offset' } in
  let range' = Range.add offset' (x, y) in
  let result' = (path', range') in
  Some result'

let use_subset (path, range) row _ =
  let dst, src, _ = row in
  let offset' = dst - src in
  let path' = { path with offset = path.offset + offset' } in
  let range' = Range.add offset' range in
  let result' = (path', range') in
  Some result'

let fold_table result table =
  let rec fold (result, table) =
    match (result, table) with
    | None, _ -> None
    | Some result, rows :: table ->
        let _, range = result in
        let fold_row row =
          let a, b = range_of row in
          let result' =
            match Range.intersect (a, b) range with
            | Some (Subset (x, y)) ->
                let result = use_subset result row (x, y) in
                result
            | Some (Overlap (x, y)) ->
                let result = use_overlap result row (x, y) in
                result
            | _ -> None
          in
          fold (result', table)
        in
        rows
        |>| List.find_opt (row_intersects range)
        |>| fun row -> Option.bind row fold_row
    | _ -> result
  in
  fold (result, table)

let compile_header header =
  let dst, src, margin = header in
  let window = (src, src + margin - 1) in
  let offset = dst - src in
  let range = Range.add offset window in
  let path = { window; offset } in
  let result = (path, range) in
  result

let compile_table : table -> t list = function
  | init :: table ->
      List.filter_map
        (fun header ->
          let result = compile_header header in
          match table |> fold_table (Some result) with
          | Some (path, _) -> Some path
          | None -> None)
        init
  | _ -> failwith "illegal"
