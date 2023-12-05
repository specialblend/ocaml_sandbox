open Str
open Fun

let r1 = Str.regexp "[0-9]"

let r2 =
  Str.regexp
    "[0-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"

let read_word = function
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | s -> int_of_string s

let read_line r l =
  let x =
    ignore (search_forward r l 0);
    read_word (matched_string l)
  and y =
    ignore (search_backward r l (String.length l));
    read_word (matched_string l)
  in
  (x * 10) + y

let read_with regex file =
  file
  |>| Core.In_channel.read_lines
  |>| List.map (read_line regex)
  |>| List.sum

let () =
  "trebuchet.txt"
  |>| Core.In_channel.read_lines
  |>| List.map (read_line r1)
  |>| List.sum
  |>| string_of_int
  |>| print_endline

let () =
  "trebuchet.txt"
  |>| Core.In_channel.read_lines
  |>| List.map (read_line r2)
  |>| List.sum
  |>| string_of_int
  |>| print_endline
