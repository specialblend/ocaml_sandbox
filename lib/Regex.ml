open Str

type t = string * int

let find_all_loc (r : regexp) (s : string) : t list =
  let _l = String.length s in
  let rec find acc i =
    try
      let i = search_forward r s i in
      let s' = matched_string s in
      let m = (s', i) in
      let i = i + String.length s' in
      find (m :: acc) i
    with
    | Not_found -> acc
  in
  find [] 0

let find_all r s = find_all_loc r s |> List.map fst
