open Fun
open Seeds

(* let _ =
   "seeds_sample.txt"
   |>| Core.In_channel.read_all
   |>| parse_all
   |>| look_seed_ranges
   |>| string_of_int
   |>| print_endline *)

let _ =
  "seeds.txt"
  |>| Core.In_channel.read_all
  |>| parse_all
  (* |>| show_table
     |>| print_endline *)
  |>| look_seed_ranges
  |>| string_of_int
  |>| print_endline

(* let _ =
   "seeds.txt"
   |>| Core.In_channel.read_all
   |>| parse_all
   |>| get_lowest_location
   |>| string_of_int
   |>| print_endline *)
