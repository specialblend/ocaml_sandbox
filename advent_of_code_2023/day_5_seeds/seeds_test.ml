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
  |>| Seeds.parse_all
  |>| Seeds.show_seed_data
  |>| print_endline;
  [%expect
    {|
    ([79; 14; 55; 13],
     [[(50, 98, 2); (52, 50, 48)]; [(0, 15, 37); (37, 52, 2); (39, 0, 15)];
       [(49, 53, 8); (0, 11, 42); (42, 0, 7); (57, 7, 4)];
       [(88, 18, 7); (18, 25, 70)]; [(45, 77, 23); (81, 45, 19); (68, 64, 13)];
       [(0, 69, 1); (1, 0, 69)]; [(60, 56, 37); (56, 93, 4)]]) |}]

let%expect_test "lookup_seed when exists" =
  let seed = 79 in
  let mappings = [ (50, 98, 2); (52, 50, 48) ] in

  Seeds.lookup_seed seed mappings |> print_int;
  [%expect {| 81 |}]

let%expect_test "lookup_seed when not exists" =
  let seed = 14 in
  let mappings = [ (50, 98, 2); (52, 50, 48) ] in

  Seeds.lookup_seed seed mappings |> print_int;
  [%expect {| 14 |}]

let%expect_test "lookup_chain 14" =
  let seed = 14 in
  let _, sections = Seeds.parse_all seeds_sample_text in

  Seeds.lookup_chain seed sections |> print_int;
  [%expect {| 43 |}]
