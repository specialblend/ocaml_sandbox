open Fun

let _ =
  let parse = Regex.find_all (Str.regexp "[0-9]+") in
  let parse =
    Str.split_by "\n"
    >> List.map parse
    >> List.filter (fun x -> List.length x > 0)
  in
  let parse = Str.split_by "\n\n" >> List.map parse in

  let print_data data =
    data
    |> List.iteri (fun section data ->
           Printf.printf "Section %d\n" (section + 1);
           data |> List.iter (String.concat "," >> print_endline))
  in

  "seeds_sample.txt" |> Core.In_channel.read_all |> parse |> print_data
