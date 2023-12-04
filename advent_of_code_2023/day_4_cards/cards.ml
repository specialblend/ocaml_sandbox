open Fun

let parse_line (line : string) =
  line
  |> Str.split (Str.regexp ":")
  |> List.nth 1
  |> Str.split (Str.regexp "|")
  |> List.map (Str.split (Str.regexp "[ ]+"))
  |> fun [ left; right ] -> (left, right)

let points n = int_of_float (2.0 ** float_of_int (n - 1))

let scan_matches (left, right) =
  List.filter (fun card -> List.exists (( = ) card) right) left

let _ =
  Core.In_channel.read_lines "cards.txt"
  |> List.map parse_line
  |> List.map scan_matches
  |> List.map List.length
  |> List.map points
  |> List.sum
  |> string_of_int
  |> print_endline
