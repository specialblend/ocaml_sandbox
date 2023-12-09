open Seeds

let%test "range intersect subset" =
  let range1 = (1, 10) in
  let range2 = (3, 5) in

  match Range.intersect range1 range2 with
  | Some (Subset (3, 5)) -> true
  | _ -> false

let%test "range intersect full subset" =
  let range1 = (1, 10) in
  let range2 = (1, 10) in

  match Range.intersect range1 range2 with
  | Some (Subset (1, 10)) -> true
  | _ -> false

let%test "range intersect superset" =
  let range1 = (3, 5) in
  let range2 = (1, 10) in

  match Range.intersect range1 range2 with
  | Some (Superset (1, 10)) -> true
  | _ -> false

let%test "range intersect overlap right" =
  let range1 = (1, 10) in
  let range2 = (4, 12) in

  match Range.intersect range1 range2 with
  | Some (Overlap (4, 10)) -> true
  | _ -> false

let%test "range intersect overlap left" =
  let range1 = (5, 13) in
  let range2 = (2, 7) in

  match Range.intersect range1 range2 with
  | Some (Overlap (5, 7)) -> true
  | _ -> false

let%test "range intersect none" =
  let range1 = (1, 13) in
  let range2 = (21, 47) in

  match Range.intersect range1 range2 with
  | None -> true
  | _ -> false
