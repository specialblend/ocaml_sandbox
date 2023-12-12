open Fun

let _ =
  "seeds.txt"
  |>| Core.In_channel.read_all
  |>| Seeds.Parser.parse_almanac
  |>| function
  | seeds, group :: _ ->
      List.map (Seeds.Lookup.compile_group group) seeds
      |>| List.map Seeds.Lookup.show_compiled_group
      |>| List.iter print_endline
  | _ -> failwith "illegal"
