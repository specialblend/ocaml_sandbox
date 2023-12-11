open Seeds
open Range

let%test "range intersect subset" =
  let range1 = Range (1, 10) in
  let range2 = Range (3, 5) in

  match Range.intersect range1 range2 with
  | Some (Subset (Range (3, 5))) -> true
  | _ -> false

let%test "range intersect full subset" =
  let range1 = Range (1, 10) in
  let range2 = Range (1, 10) in

  match Range.intersect range1 range2 with
  | Some (Subset (Range (1, 10))) -> true
  | _ -> false

let%test "range intersect superset" =
  let range1 = Range (3, 5) in
  let range2 = Range (1, 10) in

  match Range.intersect range1 range2 with
  | Some (Superset (Range (1, 10))) -> true
  | _ -> false

let%test "range intersect overlap right" =
  let range1 = Range (1, 10) in
  let range2 = Range (4, 12) in

  match Range.intersect range1 range2 with
  | Some (OverlapRight (Range (4, 10))) -> true
  | _ -> false

let%test "range intersect overlap left" =
  let range1 = Range (5, 13) in
  let range2 = Range (2, 7) in

  match Range.intersect range1 range2 with
  | Some (OverlapLeft (Range (5, 7))) -> true
  | _ -> false

let%test "range intersect none" =
  let range1 = Range (1, 13) in
  let range2 = Range (21, 47) in

  match Range.intersect range1 range2 with
  | None -> true
  | _ -> false
