open Fun
open Seeds

let%expect_test "compile_table sample" =
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Pathfinder.compile_table
  |>| List.iter (Pathfinder.show >> print_endline);
  [%expect {| { Pathfinder.window = (98, 98); offset = -31 } |}]

let%expect_test "compile_table seeds" =
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Pathfinder.compile_table
  |>| List.iter (Pathfinder.show >> print_endline);
  [%expect
    {|
    { Pathfinder.window = (1239000360, 1308640991); offset = -487996698 }
    { Pathfinder.window = (3429737174, 3434135476); offset = -1565516394 }
    { Pathfinder.window = (1535118372, 1545830629); offset = 415864339 }
    { Pathfinder.window = (438168826, 446641972); offset = 2377399210 }
    { Pathfinder.window = (536917987, 544654657); offset = 316619784 }
    { Pathfinder.window = (3429676494, 3429737173); offset = -384049208 }
    { Pathfinder.window = (979257605, 990438940); offset = 1318336963 }
    { Pathfinder.window = (3401349677, 3404859217); offset = -1031482541 }
    { Pathfinder.window = (2086137846, 2088091502); offset = -847138651 }
    { Pathfinder.window = (2683163217, 2692186170); offset = 1533326409 }
    { Pathfinder.window = (1633120598, 1662888272); offset = -1331275069 }
    { Pathfinder.window = (2940939059, 2946419136); offset = 104748907 }
    { Pathfinder.window = (498356425, 515916487); offset = 573016119 }
    { Pathfinder.window = (2711764761, 2724264424); offset = 1290124945 }
    { Pathfinder.window = (2605613670, 2623158994); offset = -2420710039 }
    { Pathfinder.window = (4294380836, 4294967295); offset = -308853639 }
    { Pathfinder.window = (3169731719, 3170733408); offset = -2810516698 }
    { Pathfinder.window = (1124514126, 1129263946); offset = 1436858815 }
    { Pathfinder.window = (773812762, 835303388); offset = 3316722590 }
    { Pathfinder.window = (20266514, 26921881); offset = 1306626526 }
    { Pathfinder.window = (1808537666, 1814135908); offset = 2276399443 }
    { Pathfinder.window = (130988760, 144724434); offset = 1523978333 }
    { Pathfinder.window = (1098613331, 1114497264); offset = 2634856385 }
    { Pathfinder.window = (858089779, 873640810); offset = 2695959987 }
    { Pathfinder.window = (2361908616, 2383690007); offset = -1747071946 }
    { Pathfinder.window = (4237225295, 4240432415); offset = -251111638 }
    { Pathfinder.window = (2253415952, 2269053206); offset = -2050966996 }
    { Pathfinder.window = (3832436687, 3847103899); offset = -1963817604 }
    { Pathfinder.window = (650502764, 657139062); offset = 2129081357 }
    { Pathfinder.window = (3970575741, 3995948346); offset = -3752489530 }
    { Pathfinder.window = (2568591486, 2604369519); offset = -1223292607 }
    { Pathfinder.window = (768686733, 773812761); offset = 2341509609 }
    { Pathfinder.window = (0, 20266513); offset = 1545059330 }
    { Pathfinder.window = (3180154163, 3192666657); offset = -1795533993 }
    { Pathfinder.window = (2776195129, 2779175480); offset = -409308345 }
    { Pathfinder.window = (1548974917, 1557270806); offset = -215426509 }
    { Pathfinder.window = (4119971124, 4145767629); offset = -2848556938 }
    { Pathfinder.window = (929849138, 957158779); offset = 3127778329 }
    { Pathfinder.window = (2667969937, 2681059372); offset = -1427017085 }
    { Pathfinder.window = (1816346942, 1895435163); offset = 797624495 }
    { Pathfinder.window = (2383690008, 2416384373); offset = 959744588 } |}]

let%test "compiled sample table is correct" =
  let _, sections = Parser.parse_almanac Parser_test.seeds_sample_text in
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Pathfinder.compile_table
  |>| List.for_all (fun path ->
          let Pathfinder.{ window; offset } = path in
          let seeds = Range.to_list window in
          let expected =
            List.map (fun seed -> Looker.look_table seed sections) seeds
          in
          let results = List.map (fun seed -> seed + offset) seeds in
          expected = results)

let%test "compiled seed table is correct" =
  let _, sections = Parser.parse_almanac Parser_test.seeds_text in
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Pathfinder.compile_table
  |>| List.for_all (fun path ->
          let Pathfinder.{ window; offset } = path in
          let x, z = window in
          let seeds = [ x + 1; x + 2; z ] in
          let expected =
            List.map (fun seed -> Looker.look_table seed sections) seeds
          in
          let results = List.map (fun seed -> seed + offset) seeds in
          expected = results)
