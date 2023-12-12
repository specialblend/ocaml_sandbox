open Fun

(* let _ =
   "seeds_sample.txt"
   |>| Core.In_channel.read_all
   |>| Seeds.Parser.parse_almanac
   |>| function
   | seeds, table ->
       List.concat_map (Seeds.Lookup.lookup_table table) seeds
       |>| List.sort compare
       |>| List.map Seeds.Parser.Mapping.show
       |>| List.iter print_endline *)

let _ =
  "seeds_sample.txt"
  |>| Core.In_channel.read_all
  |>| Seeds.Parser.parse_almanac
  |>| function
  | seeds, table ->
      List.map (Seeds.Lookup.lookup_table table) seeds
      |>| List.fold_left min max_int
      |>| string_of_int
      |>| print_endline
