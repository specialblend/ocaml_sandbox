open Fun

type cube =
  | Red of int
  | Green of int
  | Blue of int

let colon = Str.regexp ":"
and comma = Str.regexp ", "
and semicolon = Str.regexp ";"
and space = Str.regexp " "

let parse_cube = function
  | [ qty; "red" ] -> Red (int_of_string qty)
  | [ qty; "green" ] -> Green (int_of_string qty)
  | [ qty; "blue" ] -> Blue (int_of_string qty)
  | _ -> failwith "Invalid"

let parse_game =
  let parse_cubes =
    Str.split comma >> List.map (Str.split space >> parse_cube)
  in
  Str.split colon
  >> List.nth 1
  >> Str.split semicolon
  >> List.concat_map parse_cubes

let valid_cube (r, g, b) = function
  | Red qty -> qty <= r
  | Green qty -> qty <= g
  | Blue qty -> qty <= b

let valid_cubes rgb = List.for_all (valid_cube rgb)
let valid_game rgb (_, cubes) = valid_cubes rgb cubes

let max_rgb =
  List.fold_left
    (fun (r, g, b) -> function
      | Red qty -> (max r qty, g, b)
      | Green qty -> (r, max g qty, b)
      | Blue qty -> (r, g, max b qty))
    (0, 0, 0)

let power (_, cubes) =
  let r, g, b = max_rgb cubes in
  r * g * b

let read_cubes =
  Core.In_channel.read_lines
  >> List.mapi (fun id line -> (id + 1, parse_game line))

let () =
  let thresholds = (12, 13, 14) in
  read_cubes "cubes.txt"
  |> List.filter (valid_game thresholds)
  |> List.map fst
  |> List.sum
  |> (string_of_int >> print_endline)

let () =
  read_cubes "cubes.txt"
  |> List.map power
  |> List.sum
  |> (string_of_int >> print_endline)
