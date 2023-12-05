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
  left
  |> List.filter (fun card -> right |> List.exists (( = ) card))
  |> List.length

let _ =
  "cards.txt"
  |> Core.In_channel.read_lines
  |> List.map parse_line
  |> List.map count_cards
  |> List.map points
  |> List.sum
  |> string_of_int
  |> print_endline

let _print_counts counts =
  counts
  |> Array.map string_of_int
  |> Array.to_list
  |> String.concat ","
  |> print_endline

let _ =
  let cards =
    "cards.txt"
    |> Core.In_channel.read_lines
    |> List.map parse_line
    |> List.map count_cards
  in
  let counts = Array.make (List.length cards) 1 in
  let incr_by n i = counts.(i) <- counts.(i) + n in

  cards
  |> List.mapi (fun index card -> (index, card))
  |> List.iter (fun (index, count) ->
         for i = index + 1 to index + count do
           let n = counts.(index) in
           incr_by n i
         done);
  counts |> Array.to_list |> List.sum |> string_of_int |> print_endline
