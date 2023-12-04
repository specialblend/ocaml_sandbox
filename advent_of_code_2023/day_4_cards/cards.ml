open Fun

let parse_line s =
  Str.split (Str.regexp ":") s
  |> List.nth 1
  |> Str.split (Str.regexp "|")
  |> List.map (Str.split (Str.regexp "[ ]+"))
  |> fun [ left; right ] -> (left, right)

let points n = int_of_float (2.0 ** float_of_int (n - 1))

let count_cards (left, right) =
  left
  |> List.filter (fun card -> right |> List.exists (( = ) card))
  |> List.length
  |> points

let _ =
  Core.In_channel.read_lines "cards.txt"
  |> List.map parse_line
  |> List.map count_cards
  |> List.sum
  |> string_of_int
  |> print_endline
