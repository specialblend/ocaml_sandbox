open Fun

let boxes =
  [
    (1, [ "Z"; "P"; "B"; "Q"; "M"; "D"; "N" ]);
    (2, [ "V"; "H"; "D"; "M"; "Q"; "Z"; "L"; "C" ]);
    (3, [ "G"; "Z"; "F"; "V"; "D"; "R"; "H"; "Q" ]);
    (4, [ "N"; "F"; "D"; "G"; "H" ]);
    (5, [ "Q"; "F"; "N" ]);
    (6, [ "T"; "B"; "F"; "Z"; "V"; "Q"; "D" ]);
    (7, [ "H"; "S"; "V"; "D"; "Z"; "T"; "M"; "Q" ]);
    (8, [ "Q"; "N"; "P"; "F"; "G"; "M" ]);
    (9, [ "M"; "R"; "W"; "B" ]);
  ]

let parse_move move =
  let int = int_of_string in
  match Str.split (Str.regexp " ") move with
  | [ _; qty; _; src; _; dst ] -> Some (int qty, int src, int dst)
  | _ -> None

let rec move boxes (qty, src, dst) =
  match (qty, List.assoc src boxes) with
  | 0, _ -> boxes
  | _, h :: t ->
      let boxes =
        List.map
          (function
            | i, _ when i = src -> (i, t)
            | i, l when i = dst -> (i, h :: l)
            | i, l -> (i, l))
          boxes
      in
      move boxes (qty - 1, src, dst)
  | _ -> failwith "illegal move"

let _ =
  Core.In_channel.read_lines "crane_moves.txt"
  |> List.filter_map parse_move
  |> List.fold_left move boxes
  |> List.map snd
  |> List.map List.hd
  |> String.concat ""
  |> print_endline
