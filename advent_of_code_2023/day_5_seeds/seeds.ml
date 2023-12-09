open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type seed_range = int * int [@@deriving show]
type seed_col = int * int * int [@@deriving show]
type seed_row = seed_col array [@@deriving show]
type seed_table = seed_row array [@@deriving show]
type seed_map = seed_table * seed_range array [@@deriving show]

let rec chunk_pairs = function
  | x :: y :: rest -> (x, y) :: chunk_pairs rest
  | _ -> []

let parse_seeds =
  List.concat_map (List.filter_map int_of_string_opt) >> Array.of_list

let parse_seed_pairs =
  List.concat_map (List.filter_map int_of_string_opt)
  >> chunk_pairs
  >> Array.of_list

let parse_triple = function
  | [ dst; src; range ] -> Some (dst, src, range)
  | _ -> None

let parse_mapping = List.filter_map int_of_string_opt >> parse_triple
let parse_mappings = Array.map (List.filter_map parse_mapping >> Array.of_list)

let parse_sect sect =
  sect
  |>| Str.split _EOL
  |>| List.map (Regex.find_all _NUM >> List.rev)
  |>| List.filter (fun x -> List.length x > 0)

let parse_all text : seed_map =
  Str.split _EOL2 text
  |>| List.map parse_sect
  |>| function
  | h :: t -> (parse_mappings (Array.of_list t), parse_seed_pairs h)
  | _ -> failwith "illegal"

let look_row n row =
  let dst, src, range = row in
  if is_between (src, src + range) n then
    Some (dst + n - src)
  else
    None

let look_section n section =
  let rec look' res i =
    try
      let row = section.(i) in
      match look_row n row with
      | Some x -> Some x
      | None -> look' res (i + 1)
    with
    | _ -> res
  in
  let r = look' None 0 in
  match r with
  | Some x -> x
  | None -> n

let look_table n sections =
  let rec look' res i =
    try
      let section = sections.(i) in
      let res' = look_section res section in
      look' res' (i + 1)
    with
    | _ -> res
  in
  look' n 0

let look_seed_range sections (seed, offset) =
  let res = ref max_int in
  let start = seed
  and end' = seed + offset in
  let _ =
    for n = start to end' do
      let r = look_table n sections in
      print_endline (Format.sprintf "%d -> %d" n r);
      if r < !res then res := r
    done
  in
  !res

let look_seed_ranges (sections, seed_ranges) =
  let res = ref max_int in
  let _ =
    for i = 0 to Array.length seed_ranges - 1 do
      let seed_range = seed_ranges.(i) in
      let start, end' = seed_range in
      print_endline (Format.sprintf "$ %d \n" start);
      let r = look_seed_range sections seed_range in
      if r < !res then res := r;
      print_endline (Format.sprintf "# %d -> %d -> %d\n" start end' r)
    done
  in
  !res
