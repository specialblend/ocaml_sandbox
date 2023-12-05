open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

let parse_all text =
  let parse_sect sect =
    sect
    |>| Str.split _EOL
    |>| List.map (Regex.find_all _NUM)
    |>| List.filter (fun x -> List.length x > 0)
  in
  Str.split _EOL2 text |> List.map parse_sect

let print_data data =
  data
  |> List.iteri (fun section data ->
         Printf.printf "Section %d\n" (section + 1);
         data |> List.iter (String.concat "," >> print_endline))

let _ =
  "seeds_sample.txt" |> Core.In_channel.read_all |> parse_all |> print_data
