open Fun

let parse_line str =
  let colon = Str.regexp ":" in
  let pipe = Str.regexp "|" in
  let space = Str.regexp "[ ]+" in
  str
  |>| Str.split colon
  |>| List.nth 1
  |>| Str.split pipe
  |>| List.map (Str.split space)
  |>| fun [ left; right ] -> (left, right)

let points n = int_of_float (2.0 ** float_of_int (n - 1))

let count_wins (left, right) =
  left |> List.filter (List.contains right) |> List.length

let _ =
  "cards.txt"
  |>| Core.In_channel.read_lines
  |>| List.map parse_line
  |>| List.map count_wins
  |>| List.map points
  |>| List.sum
  |>| string_of_int
  |>| print_endline

let _ =
  let cards =
    Core.In_channel.read_lines "cards.txt"
    |>| List.map (parse_line >> count_wins)
  in
  let total = cards |> List.fill 1 in
  let count total (i, win_count) =
    let n = List.nth i total in
    let r = (i + 1, i + win_count) in
    total |> List.update_range (fun v -> v + n) r
  in
  cards
  |>| List.mapi pair
  |>| List.fold_left count total
  |>| List.sum
  |>| string_of_int
  |>| print_endline
