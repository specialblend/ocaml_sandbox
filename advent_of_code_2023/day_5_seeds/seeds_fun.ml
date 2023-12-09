open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type seed = int * int [@@deriving show]
type col = int * int * int [@@deriving show]
type seeds = seed list [@@deriving show]
type row = col list [@@deriving show]
type table = row list [@@deriving show]
type map = seed list * table [@@deriving show]

let rec chunk_pairs = function
  | x :: y :: rest -> (x, y) :: chunk_pairs rest
  | _ -> []

let parse_seeds =
  List.concat_map (List.filter_map int_of_string_opt) >> chunk_pairs

let parse_triple = function
  | [ dst; src; range ] -> Some (dst, src, range)
  | _ -> None

let parse_mapping = List.filter_map int_of_string_opt >> parse_triple
let parse_mappings = List.map (List.filter_map parse_mapping)

let look_row n row =
  let look n col =
    match col with
    | dst, src, range when is_between (src, src + range) n ->
        Some (dst + n - src)
    | _ -> None
  in
  let r =
    List.fold_left
      (fun acc x ->
        match acc with
        | Some x -> Some x
        | None -> look n x)
      None row
  in
  match r with
  | Some x -> x
  | None -> n

let look_table = List.fold_left look_row

let look_min (seeds, table) =
  List.map (fun seed -> look_table seed table) seeds
  |> List.fold_left min max_int

let parse_sect sect =
  sect
  |>| Str.split _EOL
  |>| List.map (Regex.find_all _NUM >> List.rev)
  |>| List.filter (fun x -> List.length x > 0)

let parse_all text : map =
  Str.split _EOL2 text
  |>| List.map parse_sect
  |>| function
  | h :: t -> (parse_seeds h, parse_mappings t)
  | _ -> failwith "illegal"
