open Seeds

module Intersect_test = struct
  let%test _ = Range.intersect (1, 10) (3, 5) = Some (Subset (3, 5))
  let%test _ = Range.intersect (1, 10) (1, 10) = Some (Subset (1, 10))
  let%test _ = Range.intersect (3, 5) (1, 10) = Some (Superset (1, 10))
  let%test _ = Range.intersect (1, 10) (4, 12) = Some (OverlapRight (4, 10))
  let%test _ = Range.intersect (5, 13) (2, 7) = Some (OverlapLeft (5, 7))
  let%test _ = Range.intersect (1, 13) (21, 47) = None
end

module Union_test = struct
  let%test _ = Range.union (1, 10) (3, 5) = Some (1, 10)
  let%test _ = Range.union (3, 5) (1, 10) = Some (1, 10)
  let%test _ = Range.union (1, 5) (5, 10) = Some (1, 10)
  let%test _ = Range.union (5, 10) (1, 5) = Some (1, 10)
  let%test _ = Range.union (1, 9) (2, 10) = Some (1, 10)
  let%test _ = Range.union (2, 10) (1, 9) = Some (1, 10)
  let%test _ = Range.union (2, 10) (1, 9) = Some (1, 10)
  let%test _ = Range.union (1, 10) (1, 10) = Some (1, 10)
  let%test _ = Range.union (1, 5) (5, 10) = Some (1, 10)
  let%test _ = Range.union (5, 10) (1, 5) = Some (1, 10)

  (*  *)
  let%test _ = Range.union_all [ (1, 10); (3, 5) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (3, 5); (1, 10) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (1, 5); (5, 10) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (5, 10); (1, 5) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (1, 9); (2, 10) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (2, 10); (1, 9) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (2, 10); (1, 9) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (1, 10); (1, 10) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (1, 5); (5, 10) ] = [ (1, 10) ]
  let%test _ = Range.union_all [ (5, 10); (1, 5) ] = [ (1, 10) ]

  (*  *)
  let%test _ =
    Range.union_all [ (1, 10); (3, 5); (20, 25); (25, 30) ]
    = [ (1, 10); (20, 30) ]

  let%test _ =
    Range.union_all [ (1, 10); (3, 5); (10, 15); (15, 20); (20, 25); (25, 30) ]
    = [ (1, 30) ]
end
