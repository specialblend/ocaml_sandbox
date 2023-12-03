open Fun

type expr = Int of int * int * int | Sym of string * int * int

let number_or_symbol = Str.regexp "\\([0-9]+\\)\\|\\([^\\.]\\)"
let scan_exprs = Regex.find_all number_or_symbol

let parse_expr y (expr, x) =
  match int_of_string_opt expr with
  | Some e -> Int (e, x, y)
  | None -> Sym (expr, x, y)

let parse_line y s = scan_exprs s |> List.map (parse_expr y)
let parse_lines = List.mapi parse_line >> List.concat_map (fun l -> l)

let _ =
  let exprs = "gondola.txt" |> Core.In_channel.read_lines |> parse_lines in
  exprs
  |> List.iter (function
       | Int (e, x, y) -> Printf.printf "%d:%d %d \n" y x e
       | Sym (e, x, y) -> Printf.printf "%d:%d %s \n" y x e)
