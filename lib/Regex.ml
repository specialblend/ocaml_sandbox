open Str

type t = string * int

let find_all (r : regexp) (s : string) : t list =
  let rec find acc i =
    try
      let i = search_forward r s i in
      let s' = matched_string s in
      let l = String.length s' in
      let i' = i + l in
      let m = (s', i) in
      find (m :: acc) i'
    with Not_found -> acc
  in
  find [] 0
