open Fun
open Parser

let smallest_int = List.fold_left min max_int

module Cursor = struct
  type t = {
    range: Range.t;
    offset: int;
  }
end

let lookup_group (group : Mapping.t list) (seed : Range.t) : Mapping.t list =
  let known =
    List.filter_map
      (fun (rg, offset) ->
        match Range.intersect seed rg with
        | Some range -> Some (range, offset)
        | _ -> None)
      group
  in
  let unknown =
    known
    |>| List.map Mapping.range
    |>| Range.diff_all seed
    |>| List.map (fun rg -> (rg, 0))
  in
  known @ unknown

let lookup_table (t : table) (s : Range.t) =
  let cursor = (s, 0) in
  let mappings =
    List.fold_left
      begin
        fun cursors group ->
          cursors
          |> List.concat_map (fun cursor ->
                 let seed, offset = cursor in
                 let cursors = lookup_group group seed in
                 List.map
                   (fun (s, o) -> (Range.map (( + ) o) s, o + offset))
                   cursors)
      end
      [ cursor ] t
  in
  mappings
  |>| List.map (fun ((x, _), offset) -> x + offset)
  |>| List.fold_left min max_int
