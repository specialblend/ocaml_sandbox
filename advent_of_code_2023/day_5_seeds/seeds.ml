open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type mapping = int * int * int [@@deriving show]
type seed = int [@@deriving show]
type seeds = seed list [@@deriving show]
type seed_data = seed list * mapping list list [@@deriving show]

let parse_seeds = List.concat_map (List.filter_map int_of_string_opt)

let parse_triple = function
  | [ dst; src; range ] -> Some (dst, src, range)
  | _ -> None

let parse_mapping = List.filter_map int_of_string_opt >> parse_triple
let parse_mappings = List.map (List.filter_map parse_mapping)

let lookup_seed_map (n : int) = function
  | dst, src, range when is_between (src, src + range) n -> Some (dst + n - src)
  | _ -> None

let lookup_seed_maps (n : int) (m : mapping list) =
  List.fold_left
    (fun acc x ->
      match acc with
      | Some x -> Some x
      | None -> lookup_seed_map n x)
    None m

let parse_sect sect =
  sect
  |>| Str.split _EOL
  |>| List.map (Regex.find_all _NUM >> List.rev)
  |>| List.filter (fun x -> List.length x > 0)

let parse_all text =
  Str.split _EOL2 text
  |>| List.map parse_sect
  |>| function
  | h :: t -> (parse_seeds h, parse_mappings t)
  | _ -> failwith "illegal"
