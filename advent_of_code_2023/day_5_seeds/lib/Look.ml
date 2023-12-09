open Fun

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

let look_row_strict n row =
  let look n col =
    match (n, col) with
    | Some n, (dst, src, range) when is_between (src, src + range) n ->
        Some (dst + n - src)
    | _ -> None
  in
  List.fold_left
    (fun acc x ->
      match acc with
      | Some x -> Some x
      | None -> look n x)
    None row

let look_table_strict = List.fold_left look_row_strict
