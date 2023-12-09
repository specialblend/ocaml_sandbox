open Fun
open Contract

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

let rec chunk_pairs = function
  | x :: y :: rest -> (x, y) :: chunk_pairs rest
  | _ -> []

let parse_seeds rows =
  rows
  |>| List.concat_map (List.filter_map int_of_string_opt)
  |>| chunk_pairs
  |>| List.sort compare

let parse_triple = function
  | [ dst; src; range ] -> Some (dst, src, range)
  | _ -> None

let parse_map = List.filter_map int_of_string_opt >> parse_triple
let parse_maps = List.map (List.filter_map parse_map)

let parse_almanac text : almanac =
  let parse_group text =
    text
    |>| Str.split _EOL
    |>| List.map (Regex.find_all _NUM >> List.rev)
    |>| List.filter (fun x -> List.length x > 0)
  in
  Str.split _EOL2 text
  |>| List.map parse_group
  |>| function
  | h :: t -> (parse_seeds h, parse_maps t)
  | _ -> failwith "illegal"
