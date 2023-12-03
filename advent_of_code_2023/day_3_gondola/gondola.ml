open Fun

type expr =
  | Int of int * int * int
  | Sym of string * int * int

let pattern = "\\([0-9]+\\)\\|\\([^\\.]\\)"
let scan_exprs = pattern |> Str.regexp |> Regex.find_all
let len = string_of_int >> String.length

let parse_expr y (expr, x) =
  match int_of_string_opt expr with
  | Some e -> Int (e, x, y)
  | None -> Sym (expr, x, y)

let parse_line y str = scan_exprs str |> List.map (parse_expr y)
let parse_lines = List.mapi parse_line >> List.concat_map (fun l -> l)

let is_adj (Int (e, x, y)) (Sym (_, x', y')) =
  let l = len e in
  let is_adj_x = is_between (x - 1, x + l)
  and is_adj_y = is_between (y - 1, y + 1) in
  is_adj_x x' && is_adj_y y'

let is_part_num board = function
  | Sym _ -> false
  | num ->
      List.exists
        (function
          | Int _ -> false
          | sym -> is_adj num sym)
        board

let parse_gear board = function
  | Sym ("*", _, _) as sym ->
      Some
        (List.filter
           (function
             | Int _ as num -> is_adj num sym
             | _ -> false)
           board)
  | _ -> None

let value = function
  | Int (e, _, _) -> e
  | _ -> 0

let _ =
  let board = "gondola.txt" |> Core.In_channel.read_lines |> parse_lines in
  board
  |> List.filter (is_part_num board)
  |> List.map value
  |> List.sum
  |> string_of_int
  |> print_endline

let _ =
  let board = "gondola.txt" |> Core.In_channel.read_lines |> parse_lines in
  board
  |> List.filter_map (parse_gear board)
  |> List.filter (List.length >> ( = ) 2)
  |> List.map (List.map value)
  |> List.map List.product
  |> List.sum
  |> string_of_int
  |> print_endline
