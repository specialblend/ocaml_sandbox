open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type mapping = int * int * int
type seed = int
type seed_data = seed list * mapping list list

let parse_seeds = List.concat_map (List.filter_map int_of_string_opt)

let parse_triple = function
  | [ src; dst; margin ] -> Some (src, dst, margin)
  | _ -> None

let parse_mapping = List.filter_map int_of_string_opt >> parse_triple
let parse_mappings = List.map (List.filter_map parse_mapping)

let parse_sect sect =
  sect
  |>| Str.split _EOL
  |>| List.map (Regex.find_all _NUM)
  |>| List.filter (fun x -> List.length x > 0)

let parse_all text : seed_data option =
  Str.split _EOL2 text
  |>| List.map parse_sect
  |>| function
  | h :: t -> Some (parse_seeds h, parse_mappings t)
  | _ -> None

let _ =
  "seeds_sample.txt"
  |>| Core.In_channel.read_all
  |>| parse_all
  |>| Option.map (fun x -> x)
