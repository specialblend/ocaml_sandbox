open Fun

let parse_line y str =
  let exp = "\\([0-9]+\\)\\|\\([^\\.]\\)" in
  Regex.find_all (Str.regexp exp) str |> List.map (fun (e, x) -> (e, (x, y)))

let evaluate (e, _) = int_of_string_opt e
let is_sym (e, _) = is_numeric e
let is_num (e, l) = not (is_sym (e, l))
let is_gear_symbol (e, _) = e = "*"

let is_adjacent expr1 expr2 =
  let e, (x, y) = expr1
  and e', (x', y') = expr2 in
  is_between (x - String.length e', x + String.length e) x'
  && is_between (y - 1, y + 1) y'

let read_board filename =
  Core.In_channel.read_lines filename |> List.mapi parse_line |> List.flatten

let _ =
  let board = read_board "gondola.txt" in
  let symbols = List.filter is_sym board in
  List.filter is_num board
  |> List.filter (fun number -> List.exists (is_adjacent number) symbols)
  |> List.filter_map evaluate
  |> List.sum
  |> string_of_int
  |> print_endline

let _ =
  let board = read_board "gondola.txt" in
  let numbers = List.filter is_num board in
  List.filter is_gear_symbol board
  |> List.map (fun symbol -> List.filter (is_adjacent symbol) numbers)
  |> List.filter (fun parts -> List.length parts = 2)
  |> List.map (List.filter_map evaluate)
  |> List.map List.product
  |> List.sum
  |> string_of_int
  |> print_endline
