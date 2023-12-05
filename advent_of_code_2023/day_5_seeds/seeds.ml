open Fun

let _ =
  let parse_line = Regex.find_all (Str.regexp "[0-9]+") >> List.map fst in
  let parse_section =
    Str.split_by "\n"
    >> List.map parse_line
    >> List.filter (fun x -> List.length x > 0)
  in
  let parse_file = Str.split_by "\n\n" >> List.map parse_section in

  let print_data data =
    data
    |> List.iteri (fun section data ->
           Printf.printf "Section %d\n" (section + 1);
           data |> List.iter (String.concat "," >> print_endline))
  in

  "seeds_sample.txt" |> Core.In_channel.read_all |> parse_file |> print_data
