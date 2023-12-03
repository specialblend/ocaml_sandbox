open Fun

type expr =
  | Int of int * int * int
  | Sym of string * int * int

let scan_board = Regex.find_all (Str.regexp "\\([0-9]+\\)\\|\\([^\\.]\\)")

let parse_expr y (expr, x) =
  match int_of_string_opt expr with
  | Some e -> Int (e, x, y)
  | None -> Sym (expr, x, y)

let parse_line y s = scan_board s |> List.map (parse_expr y)
let parse_lines = List.mapi parse_line >> List.concat_map (fun l -> l)

(*  *)
let is_between (min, max) x = x >= min && x <= max

let is_part_num (board : expr list) =
  let is_adjacent (e, x, y) =
    let len = String.length (string_of_int e) in
    let is_adj_x = is_between (x - 1, x + len)
    and is_adj_y = is_between (y - 1, y + 1) in
    function
    | Sym (_, x', y') -> is_adj_x x' && is_adj_y y'
    | _ -> false
  in
  function
  | Sym (_, _, _) -> false
  | Int (e, x, y) -> List.exists (is_adjacent (e, x, y)) board

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
