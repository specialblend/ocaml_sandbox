open Fun
open Seeds_fun

let _ =
  "seeds_sample.txt"
  |>| Core.In_channel.read_all
  |>| parse_all
  |>| show_seed_data
  |>| print_endline

let _ =
  "seeds.txt"
  |>| Core.In_channel.read_all
  |>| parse_all
  |>| get_lowest_location
  |>| string_of_int
  |>| print_endline
