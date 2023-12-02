type cube = Red of int | Green of int | Blue of int [@@deriving show]
type game = int * cube list [@@deriving show]

open Str

let ( >> ) f g x = g (f x)
let nth n l = List.nth l n
let int = int_of_string
let re = regexp
let sum = List.fold_left ( + ) 0

let colon = re ":"
and comma = re ", "
and semicolon = re ";"
and space = re " "

let parse_cube = function
  | [ qty; "red" ] -> Red (int qty)
  | [ qty; "green" ] -> Green (int qty)
  | [ qty; "blue" ] -> Blue (int qty)

let parse_game =
  let parse_sets = split colon >> nth 1 >> split semicolon in
  let parse_set = split comma >> List.map (split space >> parse_cube) in
  parse_sets >> List.concat_map parse_set

let valid_cube (max_red, max_green, max_blue) = function
  | Red qty -> qty <= max_red
  | Green qty -> qty <= max_green
  | Blue qty -> qty <= max_blue

let valid_cubes thresholds = List.for_all (valid_cube thresholds)
let valid_game thresholds (_, cubes) = valid_cubes thresholds cubes

let max_rgb =
  let init = (0, 0, 0) in
  List.fold_left
    (fun (r, g, b) -> function
      | Red qty -> (max r qty, g, b)
      | Green qty -> (r, max g qty, b)
      | Blue qty -> (r, g, max b qty))
    init

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
