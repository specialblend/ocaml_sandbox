open Fun

let _ =
  "seeds.txt"
  |>| Core.In_channel.read_all
  |>| Seeds.Parser.parse_almanac
  |>| fun (seeds, table) ->
  List.map (Seeds.Lookup.smallest_in_table table) seeds
  |>| List.fold_left min max_int
  |>| string_of_int
  |>| print_endline
