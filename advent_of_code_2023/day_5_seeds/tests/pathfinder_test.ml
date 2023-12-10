open Fun
open Seeds
open Seeds.Pathfinder

let%expect_test "compile_paths sample" =
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Pathfinder.compile_paths
  |>| List.iter (Path.show >> print_endline);
  [%expect {| |}]

let%expect_test "compile_paths seeds" =
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Pathfinder.compile_paths
  |>| List.iter (Path.show >> print_endline);
  [%expect
    {|
    { domain = (1, 20266513); offset = 1545059330 }
    { domain = (20266515, 26921881); offset = 1306626526 }
    { domain = (130988761, 144724434); offset = 1523978333 }
    { domain = (438168827, 446641972); offset = 2377399210 }
    { domain = (498356426, 515916487); offset = 573016119 }
    { domain = (536917988, 544654657); offset = 316619784 }
    { domain = (650502765, 657139062); offset = 2129081357 }
    { domain = (768686734, 773812761); offset = 2341509609 }
    { domain = (773812763, 835303388); offset = 3316722590 }
    { domain = (858089780, 873640810); offset = 2695959987 }
    { domain = (929849139, 957158779); offset = 3127778329 }
    { domain = (979257606, 990438940); offset = 1318336963 }
    { domain = (1098613332, 1114497264); offset = 2634856385 }
    { domain = (1124514127, 1129263946); offset = 1436858815 }
    { domain = (1239000361, 1308640991); offset = -487996698 }
    { domain = (1535118373, 1545830629); offset = 415864339 }
    { domain = (1548974918, 1557270806); offset = -215426509 }
    { domain = (1633120599, 1662888272); offset = -1331275069 }
    { domain = (1808537667, 1814135908); offset = 2276399443 }
    { domain = (1816346943, 1895435163); offset = 797624495 }
    { domain = (2086137847, 2088091502); offset = -847138651 }
    { domain = (2253415953, 2269053206); offset = -2050966996 }
    { domain = (2361908617, 2383690007); offset = -1747071946 }
    { domain = (2383690009, 2416384373); offset = 959744588 }
    { domain = (2568591487, 2604369519); offset = -1223292607 }
    { domain = (2605613671, 2623158994); offset = -2420710039 }
    { domain = (2667969938, 2681059372); offset = -1427017085 }
    { domain = (2683163218, 2692186170); offset = 1533326409 }
    { domain = (2711764762, 2724264424); offset = 1290124945 }
    { domain = (2776195130, 2779175480); offset = -409308345 }
    { domain = (2940939060, 2946419136); offset = 104748907 }
    { domain = (3169731720, 3170733408); offset = -2810516698 }
    { domain = (3180154164, 3192666657); offset = -1795533993 }
    { domain = (3401349678, 3404859217); offset = -1031482541 }
    { domain = (3429676495, 3429737173); offset = -384049208 }
    { domain = (3429737175, 3434135476); offset = -1565516394 }
    { domain = (3832436688, 3847103899); offset = -1963817604 }
    { domain = (3970575742, 3995948346); offset = -3752489530 }
    { domain = (4119971125, 4145767629); offset = -2848556938 }
    { domain = (4237225296, 4240432415); offset = -251111638 }
    { domain = (4294380837, 4294967295); offset = -308853639 } |}]

let%test "compiled sample table is correct" =
  let _, table = Parser.parse_almanac Parser_test.seeds_sample_text in
  table
  |>| Pathfinder.compile_paths
  |>| List.for_all (fun path ->
          let Path.{ domain; offset } = path in
          let seeds = Range.to_list domain in
          let expected =
            List.map (fun seed -> Looker.look_table seed table) seeds
          in
          let results = List.map (fun seed -> seed + offset) seeds in
          expected = results)

let%test "compiled seed table is correct" =
  let _, table = Parser.parse_almanac Parser_test.seeds_text in
  table
  |>| Pathfinder.compile_paths
  |>| List.for_all (fun path ->
          let Path.{ domain; offset } = path in
          let x, z = domain in
          let y = (x + z) / 2 in
          let seeds = [ x; y; z ] in
          let expected =
            List.map (fun seed -> Looker.look_table seed table) seeds
          in
          let results = List.map (fun seed -> seed + offset) seeds in
          expected = results)

let%expect_test "compile_known_seeds" =
  let seeds, table = Parser.parse_almanac Parser_test.seeds_text in
  seeds
  |>| compile_known_seeds table
  |>| List.map show_known_seed
  |>| List.iter print_endline;
  [%expect
    {|
    { seed = (41218238, 421491713); range = (41218238, 462709951);
      path = { domain = (130988761, 144724434); offset = 1523978333 };
      intersect = (Superset (41218238, 462709951)) }
    { seed = (481818804, 233571979); range = (481818804, 715390783);
      path = { domain = (498356426, 515916487); offset = 573016119 };
      intersect = (Superset (481818804, 715390783)) }
    { seed = (944138913, 251104806); range = (944138913, 1195243719);
      path = { domain = (929849139, 957158779); offset = 3127778329 };
      intersect = (Overlap (944138913, 957158779)) }
    { seed = (1255413673, 350530906); range = (1255413673, 1605944579);
      path = { domain = (1239000361, 1308640991); offset = -487996698 };
      intersect = (Overlap (1255413673, 1308640991)) }
    { seed = (2109326496, 538709762); range = (2109326496, 2648036258);
      path = { domain = (2253415953, 2269053206); offset = -2050966996 };
      intersect = (Superset (2109326496, 2648036258)) }
    { seed = (2906248740, 266447632); range = (2906248740, 3172696372);
      path = { domain = (2940939060, 2946419136); offset = 104748907 };
      intersect = (Superset (2906248740, 3172696372)) }
    { seed = (3579244700, 267233350); range = (3579244700, 3846478050);
      path = { domain = (3832436688, 3847103899); offset = -1963817604 };
      intersect = (Overlap (3832436688, 3846478050)) } |}]

let%expect_test "print lookup" =
  let seeds, table = Parser.parse_almanac Parser_test.seeds_text in
  List.iter
    (fun (start, _) ->
      let n = Looker.look_table start table in
      Printf.printf "%d -> %d\n" start n)
    seeds;
  [%expect
    {|
    41218238 -> 3550589567
    481818804 -> 3860266581
    944138913 -> 4071917242
    1255413673 -> 767416975
    1920342932 -> 2591030530
    2109326496 -> 1471140253
    2906248740 -> 1762824436
    3454130719 -> 2613413552
    3579244700 -> 2395376000
    4173137165 -> 251346198 |}]

(* let%test "compile_known_seeds verification" =
   let seeds, table = Parser.parse_almanac Parser_test.seeds_text in
   let verify_known_seed (seed, offset) =
     let x, z = seed in
     let _y = (x + z) / 2 in
     let seeds = [ x + 1 ] in
     List.for_all
       (fun seed ->
         let expected = Looker.look_table seed table in
         let result = seed + offset in
         expected = result)
       seeds
   in
   seeds |>| compile_known_seeds table |>| List.for_all verify_known_seed *)
