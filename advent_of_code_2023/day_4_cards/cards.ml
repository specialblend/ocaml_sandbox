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
  |>| function
  | [ left; right ] -> (left, right)
  | _ -> failwith "bad input"

let points n = int_of_float (2.0 ** float_of_int (n - 1))

let count_wins (left_cards, right_cards) =
  left_cards
  |>| List.filter (fun card -> right_cards |>| List.mem card)
  |>| List.length

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
    "cards.txt"
    |>| Core.In_channel.read_lines
    |>| List.map (parse_line >> count_wins)
  in
  let counter = cards |> List.fill 1 |> Array.of_list in
  let count_copies counter (i, win_count) =
    let n = counter.(i) in
    for k = i + 1 to i + win_count do
      counter.(k) <- counter.(k) + n
    done;
    counter
  in
  cards
  |>| List.mapi pair
  |>| List.fold_left count_copies counter
  |>| Array.sum
  |>| string_of_int
  |>| print_endline
