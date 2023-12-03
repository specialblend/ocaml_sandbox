open Fun

type expr =
  | Int of int * int * int
  | Sym of string * int * int

let number_or_symbol = Str.regexp "\\([0-9]+\\)\\|\\([^\\.]\\)"
let scan_board = Regex.find_all number_or_symbol

let parse_expr y (expr, x) =
  match int_of_string_opt expr with
  | Some e -> Int (e, x, y)
  | None -> Sym (expr, x, y)

let parse_line y s = scan_board s |> List.map (parse_expr y)
let parse_lines = List.mapi parse_line >> List.concat_map (fun l -> l)

let _print_expr = function
  | Int (e, x, y) -> Printf.printf "%d:%d %d \n" y x e
  | Sym (e, x, y) -> Printf.printf "%d:%d %s \n" y x e

let is_part_num (board : expr list) = function
  | Int (_, _x, _y) -> List.exists (fun _ -> true) board
  | Sym (_, _, _) -> false

let value = function
  | Int (e, _, _) -> e
  | Sym (_, _, _) -> 0

let _ =
  let board = "gondola.txt" |> Core.In_channel.read_lines |> parse_lines in
  board
  |> List.filter (is_part_num board)
  |> List.map value
  |> List.fold_left ( + ) 0
  |> string_of_int
  |> print_endline
