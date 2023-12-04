open Fun

let nth n l = List.nth l n
let parse_nums = Str.split (Str.regexp "[ ]+")

let parse_line (line : string) =
  line
  |> Str.split (Str.regexp ":")
  |> nth 1
  |> Str.split (Str.regexp "|")
  |> List.map parse_nums
  |> fun [ a; b ] -> (a, b)

let scan_matches (cards, numbers) =
  List.filter (fun card -> List.mem card numbers) cards

let points n = 2.0 ** float_of_int (n - 1) |> int_of_float

let _ =
  Core.In_channel.read_lines "cards.txt"
  |> List.map parse_line
  |> List.map scan_matches
  |> List.map List.length
  |> List.map points
  |> List.sum
  |> string_of_int
  |> print_endline
