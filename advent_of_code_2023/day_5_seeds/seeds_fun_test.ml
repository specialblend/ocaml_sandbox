open Fun

let seeds_sample_text =
  {|

seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}

let%expect_test "parse_all" =
  seeds_sample_text
  |>| Seeds_fun.parse_almanac
  |>| Seeds_fun.show_almanac
  |>| print_endline;
  [%expect
    {|
    ([(79, 14); (55, 13)],
     [[(50, 98, 2); (52, 50, 48)]; [(0, 15, 37); (37, 52, 2); (39, 0, 15)];
       [(49, 53, 8); (0, 11, 42); (42, 0, 7); (57, 7, 4)];
       [(88, 18, 7); (18, 25, 70)]; [(45, 77, 23); (81, 45, 19); (68, 64, 13)];
       [(0, 69, 1); (1, 0, 69)]; [(60, 56, 37); (56, 93, 4)]]) |}]

let%expect_test "look_row when exists" =
  let seed = 79 in
  let mappings = [ (50, 98, 2); (52, 50, 48) ] in

  Seeds_fun.look_row seed mappings |> print_int;
  [%expect {| 81 |}]

let%expect_test "look_row when not exists" =
  let seed = 14 in
  let mappings = [ (50, 98, 2); (52, 50, 48) ] in

  Seeds_fun.look_row seed mappings |> print_int;
  [%expect {| 14 |}]

let%expect_test "look_table 14" =
  let seed = 14 in
  let _, sections = Seeds_fun.parse_almanac seeds_sample_text in

  Seeds_fun.look_table seed sections |> print_int;
  [%expect {| 43 |}]

let%expect_test "map seeds to final numbers" =
  let seeds = [ 79; 14; 55; 13 ] in
  let _, sections = Seeds_fun.parse_almanac seeds_sample_text in

  List.map (fun seed -> Seeds_fun.look_table seed sections) seeds
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline;
  [%expect {| 82 43 86 35 |}]

let%expect_test "look_min" =
  let seeds = [ 79; 14; 55; 13 ] in
  let _, sections = Seeds_fun.parse_almanac seeds_sample_text in

  Seeds_fun.look_min (seeds, sections) |> string_of_int |> print_endline;
  [%expect {| 35 |}]

let%test "range intersect subset" =
  let range1 = (1, 10) in
  let range2 = (3, 5) in

  match Seeds_fun.Range.intersect range1 range2 with
  | Some (Subset (3, 5)) -> true
  | _ -> false

let%test "range intersect full subset" =
  let range1 = (1, 10) in
  let range2 = (1, 10) in

  match Seeds_fun.Range.intersect range1 range2 with
  | Some (Subset (1, 10)) -> true
  | _ -> false

let%test "range intersect superset" =
  let range1 = (3, 5) in
  let range2 = (1, 10) in

  match Seeds_fun.Range.intersect range1 range2 with
  | Some (Superset (1, 10)) -> true
  | _ -> false

let%test "range intersect overlap right" =
  let range1 = (1, 10) in
  let range2 = (4, 12) in

  match Seeds_fun.Range.intersect range1 range2 with
  | Some (Overlap (4, 10)) -> true
  | _ -> false

let%test "range intersect overlap left" =
  let range1 = (5, 13) in
  let range2 = (2, 7) in

  match Seeds_fun.Range.intersect range1 range2 with
  | Some (Overlap (5, 7)) -> true
  | _ -> false

let%test "range intersect none" =
  let range1 = (1, 13) in
  let range2 = (21, 47) in

  match Seeds_fun.Range.intersect range1 range2 with
  | None -> true
  | _ -> false
