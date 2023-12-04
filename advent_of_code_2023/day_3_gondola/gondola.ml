open Fun

type expr = string * int * int

let parse_line (y : int) str : expr list =
  str
  |> Regex.find_all (Str.regexp "\\([0-9]+\\)\\|\\([^\\.]\\)")
  |> List.map (fun (e, x) -> (e, x, y))

let len = String.length
let is_num (e, _, _) = int_of_string_opt e <> None
let is_sym (e, _, _) = int_of_string_opt e = None

let is_adj (e, x, y) (e', x', y') =
  is_between (x - len e', x + len e) x' && is_between (y - 1, y + 1) y'

let is_part_num board num = List.exists (is_adj num) (List.filter is_sym board)

let parse_gear board = function
  | ("*", _, _) as gear -> List.filter (is_adj gear) (List.filter is_num board)
  | _ -> []

let read_board =
  Core.In_channel.read_lines >> List.mapi parse_line >> List.flatten

let _ =
  let board = read_board "gondola.txt" in
  List.filter is_num board
  |> List.filter (is_part_num board)
  |> List.filter_map (fun (e, _, _) -> int_of_string_opt e)
  |> List.sum
  |> string_of_int
  |> print_endline

let _ =
  let board = read_board "gondola.txt" in
  board
  |> List.map (parse_gear board)
  |> List.filter (List.length >> ( = ) 2)
  |> List.map (List.filter_map (fun (e, _, _) -> int_of_string_opt e))
  |> List.map List.product
  |> List.sum
  |> string_of_int
  |> print_endline
