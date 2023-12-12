open Seeds

module Intersect_test = struct
  let%test _ = Range.intersect (1, 10) (3, 5) = Some (3, 5)
  let%test _ = Range.intersect (1, 10) (1, 10) = Some (1, 10)
  let%test _ = Range.intersect (3, 5) (1, 10) = Some (3, 5)
  let%test _ = Range.intersect (1, 10) (4, 12) = Some (4, 10)
  let%test _ = Range.intersect (5, 13) (2, 7) = Some (5, 7)
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

module Diff_test = struct
  let%test _ = Range.diff (3, 5) (1, 10) = []
  let%test _ = Range.diff (1, 10) (1, 10) = []
  let%test _ = Range.diff (1, 10) (3, 5) = [ (1, 2); (6, 10) ]
  let%test _ = Range.diff (1, 5) (5, 10) = [ (1, 4) ]
  let%test _ = Range.diff (5, 10) (1, 5) = [ (6, 10) ]
  let%test _ = Range.diff (5, 10) (1, 9) = [ (10, 10) ]
  let%test _ = Range.diff (1, 9) (3, 10) = [ (1, 2) ]
  let%test _ = Range.diff (2, 10) (1, 9) = [ (10, 10) ]
  let%test _ = Range.diff (50, 100) (25, 75) = [ (76, 100) ]

  (*  *)
  let%test _ = Range.diff_all (1, 20) [ (5, 7) ] = [ (1, 4); (8, 20) ]

  let%test _ =
    let ranges = [ (5, 7); (12, 15) ]
    and expected = [ (1, 4); (8, 11); (16, 20) ] in
    Range.diff_all (1, 20) ranges = expected

  let%test _ = Range.diff_all (1, 100) [ (1, 10); (90, 100) ] = [ (11, 89) ]

  let%test _ =
    let ranges = [ (10, 20); (30, 40); (50, 60); (70, 80); (90, 100) ]
    and expected = [ (1, 9); (21, 29); (41, 49); (61, 69); (81, 89) ] in
    Range.diff_all (1, 100) ranges = expected

  let%test _ =
    let ranges =
      [ (10, 20); (30, 40); (50, 60); (35, 37); (70, 80); (90, 100); (11, 15) ]
    and expected = [ (1, 9); (21, 29); (41, 49); (61, 69); (81, 89) ] in
    Range.diff_all (1, 100) ranges = expected

  let%expect_test _ =
    let ranges = [ (10, 20); (30, 40); (50, 60); (70, 80); (90, 100) ] in
    Range.diff_all (1, 100) ranges
    |> List.map Range.show
    |> List.iter print_endline;
    [%expect
      {|
      (1, 9)
      (21, 29)
      (41, 49)
      (61, 69)
      (81, 89) |}]
end
