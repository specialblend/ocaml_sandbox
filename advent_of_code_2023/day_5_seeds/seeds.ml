open Fun

let _NUM = Str.regexp "[0-9]+"
and _EOL = Str.regexp "\n"
and _EOL2 = Str.regexp "\n\n"

type table = int array * (int * int * int) array array [@@deriving show]

let parse_seeds =
  List.concat_map (List.filter_map int_of_string_opt) >> Array.of_list

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

let parse_all text =
  Str.split _EOL2 text
  |>| List.map parse_sect
  |>| function
  | h :: t -> (parse_seeds h, parse_mappings (Array.of_list t))
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

(* let look_table n sections =
   let rec look' res i =
     try
       let section = sections.(i) in
       let res' = look_section res section in
       look' res' (i + 1)
     with
     | _ -> res
   in
   look' n 0 *)

let look_table n sections =
  let res = ref n in
  let _ =
    for i = 0 to Array.length sections - 1 do
      let section = sections.(i) in
      res := look_section !res section
    done
  in
  !res
