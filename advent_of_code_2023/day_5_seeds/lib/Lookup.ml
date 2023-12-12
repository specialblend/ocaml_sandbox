open Fun
open Parser

let smallest_int = List.fold_left min max_int

type compiled_group = {
  seed: seed;
  known: (Range.t * int) list;
  unknown: (Range.t * int) list;
}
[@@deriving show]

let compile_group (group : Mapping.t list) (s : seed) =
  let known =
    List.filter_map
      (fun (rg, offset) ->
        match Range.intersect (Mapping.range s) rg with
        | Some rg -> Some (rg, offset)
        | _ -> None)
      group
  in
  let unknown =
    known
    |>| List.map Mapping.range
    |>| Range.diff_all (Mapping.range s)
    |>| List.map (fun rg -> (rg, 0))
  in
  { seed = s; known; unknown }
