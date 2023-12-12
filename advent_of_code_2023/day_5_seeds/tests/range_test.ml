open Seeds

module Intersect_tests = struct
  let%test _ = Range.intersect (1, 10) (3, 5) = Some (Subset (3, 5))
  let%test _ = Range.intersect (1, 10) (1, 10) = Some (Subset (1, 10))
  let%test _ = Range.intersect (3, 5) (1, 10) = Some (Superset (1, 10))
  let%test _ = Range.intersect (1, 10) (4, 12) = Some (OverlapRight (4, 10))
  let%test _ = Range.intersect (5, 13) (2, 7) = Some (OverlapLeft (5, 7))
  let%test _ = Range.intersect (1, 13) (21, 47) = None
end

module Union_tests = struct
  (* let%test _ = Range.union (1, 10) (3, 5) = Some (1, 10) *)
end
