open Fun
open Seeds

let _ =
  "seeds_sample.txt"
  |>| Core.In_channel.read_all
  |>| parse_all
  |>| show_seed_data
  |>| print_endline

let _ =
  "seeds_sample.txt"
  |>| Core.In_channel.read_all
  |>| parse_all
  |>| show_seed_data
  |>| print_endline
