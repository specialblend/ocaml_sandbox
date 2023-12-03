type expr = Int of int * int * int | Sym of string * int * int

let scan_exprs = Regex.find_all (Str.regexp "\\([0-9]+\\)\\|\\([^\\.]\\)")

let parse_expr y (expr, x) =
  match int_of_string_opt expr with
  | Some e -> Int (e, x, y)
  | None -> Sym (expr, x, y)

let parse_line y s = scan_exprs s |> List.map (parse_expr y)

let _ =
  let exprs =
    Core.In_channel.read_lines "gondola.txt"
    |> List.mapi parse_line
    |> List.concat_map (fun l -> l)
  in
  exprs
  |> List.iter (function
       | Int (e, x, y) -> Printf.printf "%d:%d %d \n" y x e
       | Sym (e, x, y) -> Printf.printf "%d:%d %s \n" y x e)
