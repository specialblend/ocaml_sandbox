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
    { Pathfinder.Path.domain = (1, 20266513); offset = 1545059330 }
    { Pathfinder.Path.domain = (20266515, 26921881); offset = 1306626526 }
    { Pathfinder.Path.domain = (130988761, 144724434); offset = 1523978333 }
    { Pathfinder.Path.domain = (438168827, 446641972); offset = 2377399210 }
    { Pathfinder.Path.domain = (498356426, 515916487); offset = 573016119 }
    { Pathfinder.Path.domain = (536917988, 544654657); offset = 316619784 }
    { Pathfinder.Path.domain = (650502765, 657139062); offset = 2129081357 }
    { Pathfinder.Path.domain = (768686734, 773812761); offset = 2341509609 }
    { Pathfinder.Path.domain = (773812763, 835303388); offset = 3316722590 }
    { Pathfinder.Path.domain = (858089780, 873640810); offset = 2695959987 }
    { Pathfinder.Path.domain = (929849139, 957158779); offset = 3127778329 }
    { Pathfinder.Path.domain = (979257606, 990438940); offset = 1318336963 }
    { Pathfinder.Path.domain = (1098613332, 1114497264); offset = 2634856385 }
    { Pathfinder.Path.domain = (1124514127, 1129263946); offset = 1436858815 }
    { Pathfinder.Path.domain = (1239000361, 1308640991); offset = -487996698 }
    { Pathfinder.Path.domain = (1535118373, 1545830629); offset = 415864339 }
    { Pathfinder.Path.domain = (1548974918, 1557270806); offset = -215426509 }
    { Pathfinder.Path.domain = (1633120599, 1662888272); offset = -1331275069 }
    { Pathfinder.Path.domain = (1808537667, 1814135908); offset = 2276399443 }
    { Pathfinder.Path.domain = (1816346943, 1895435163); offset = 797624495 }
    { Pathfinder.Path.domain = (2086137847, 2088091502); offset = -847138651 }
    { Pathfinder.Path.domain = (2253415953, 2269053206); offset = -2050966996 }
    { Pathfinder.Path.domain = (2361908617, 2383690007); offset = -1747071946 }
    { Pathfinder.Path.domain = (2383690009, 2416384373); offset = 959744588 }
    { Pathfinder.Path.domain = (2568591487, 2604369519); offset = -1223292607 }
    { Pathfinder.Path.domain = (2605613671, 2623158994); offset = -2420710039 }
    { Pathfinder.Path.domain = (2667969938, 2681059372); offset = -1427017085 }
    { Pathfinder.Path.domain = (2683163218, 2692186170); offset = 1533326409 }
    { Pathfinder.Path.domain = (2711764762, 2724264424); offset = 1290124945 }
    { Pathfinder.Path.domain = (2776195130, 2779175480); offset = -409308345 }
    { Pathfinder.Path.domain = (2940939060, 2946419136); offset = 104748907 }
    { Pathfinder.Path.domain = (3169731720, 3170733408); offset = -2810516698 }
    { Pathfinder.Path.domain = (3180154164, 3192666657); offset = -1795533993 }
    { Pathfinder.Path.domain = (3401349678, 3404859217); offset = -1031482541 }
    { Pathfinder.Path.domain = (3429676495, 3429737173); offset = -384049208 }
    { Pathfinder.Path.domain = (3429737175, 3434135476); offset = -1565516394 }
    { Pathfinder.Path.domain = (3832436688, 3847103899); offset = -1963817604 }
    { Pathfinder.Path.domain = (3970575742, 3995948346); offset = -3752489530 }
    { Pathfinder.Path.domain = (4119971125, 4145767629); offset = -2848556938 }
    { Pathfinder.Path.domain = (4237225296, 4240432415); offset = -251111638 }
    { Pathfinder.Path.domain = (4294380837, 4294967295); offset = -308853639 } |}]

let%test "compiled sample table is correct" =
  let _, table = Parser.parse_almanac Parser_test.seeds_sample_text in
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
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
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
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
