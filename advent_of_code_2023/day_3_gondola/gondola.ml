open Fun

let parse_line y str =
  Regex.find_all (Str.regexp "\\([0-9]+\\)\\|\\([^\\.]\\)") str
  |> List.map (fun (e, x) -> (e, (x, y)))

let len = String.length
let is_sym (e, _) = int_of_string_opt e = None
let is_num (e, l) = not (is_sym (e, l))

let is_adj (e, (x, y)) (e', (x', y')) =
  is_between (x - len e', x + len e) x' && is_between (y - 1, y + 1) y'

let is_part_num board num = List.exists (is_adj num) (List.filter is_sym board)
let parse_gear board gear = List.filter (is_adj gear) (List.filter is_num board)

let read_board =
  Core.In_channel.read_lines >> List.mapi parse_line >> List.flatten

let _ =
  let board = read_board "gondola.txt" in
  List.filter is_num board
  |> List.filter (is_part_num board)
  |> List.filter_map (fst >> int_of_string_opt)
  |> List.sum
  |> string_of_int
  |> print_endline

let _ =
  let board = read_board "gondola.txt" in
  List.filter
    (function
      | "*", _ -> true
      | _ -> false)
    board
  |> List.map (parse_gear board)
  |> List.filter (List.length >> ( = ) 2)
  |> List.map (List.filter_map (fst >> int_of_string_opt))
  |> List.map List.product
  |> List.sum
  |> string_of_int
  |> print_endline
