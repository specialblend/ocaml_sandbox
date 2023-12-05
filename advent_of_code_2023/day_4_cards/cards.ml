open Fun

let parse_line str =
  let colon = Str.regexp ":" in
  let pipe = Str.regexp "|" in
  let space = Str.regexp "[ ]+" in
  str
  |> Str.split colon
  |> List.nth 1
  |> Str.split pipe
  |> List.map (Str.split space)
  |> fun [ left; right ] -> (left, right)

let points n = int_of_float (2.0 ** float_of_int (n - 1))

let count_cards (left, right) =
  left |> List.filter (fun card -> right |> List.mem card) |> List.length

let _ =
  "cards.txt"
  |> Core.In_channel.read_lines
  |> List.map parse_line
  |> List.map count_cards
  |> List.map points
  |> List.sum
  |> string_of_int
  |> print_endline

let _ =
  let cards =
    "cards.txt"
    |> Core.In_channel.read_lines
    |> List.map parse_line
    |> List.map count_cards
  in

  let init_counter = Array.make (List.length cards) 1 in

  let count_copies counter (index, count) =
    let n = counter.(index) in
    let _ =
      for k = index + 1 to index + count do
        counter.(k) <- counter.(k) + n
      done
    in
    counter
  in

  cards
  |> List.mapi pair
  |> List.fold_left count_copies init_counter
  |> Array.to_list
  |> List.sum
  |> string_of_int
  |> print_endline
