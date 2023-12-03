open Str
open Fun

type cube = Red of int | Green of int | Blue of int
type game = int * cube list

let nth n l = List.nth l n
let sum = List.fold_left ( + ) 0

let colon = regexp ":"
and comma = regexp ", "
and semicolon = regexp ";"
and space = regexp " "

let parse_cube = function
  | [ qty; "red" ] -> Red (int_of_string qty)
  | [ qty; "green" ] -> Green (int_of_string qty)
  | [ qty; "blue" ] -> Blue (int_of_string qty)

let parse_game =
  let parse_sets = split colon >> nth 1 >> split semicolon in
  let parse_cubes = split comma >> List.map (split space >> parse_cube) in
  parse_sets >> List.concat_map parse_cubes

let valid_cube (r, g, b) = function
  | Red qty -> qty <= r
  | Green qty -> qty <= g
  | Blue qty -> qty <= b

let valid_cubes thresholds = List.for_all (valid_cube thresholds)
let valid_game thresholds (_, cubes) = valid_cubes thresholds cubes

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

let () =
  let thresholds = (12, 13, 14) in
  Core.In_channel.read_lines "cubes.txt"
  |> List.mapi (fun id line -> (id + 1, parse_game line))
  |> List.filter (valid_game thresholds)
  |> List.map fst |> sum |> print_int |> print_newline

let () =
  Core.In_channel.read_lines "cubes.txt"
  |> List.mapi (fun id line -> (id + 1, parse_game line))
  |> List.map power |> sum |> print_int |> print_newline
