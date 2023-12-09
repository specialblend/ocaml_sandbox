open Fun
open Seeds

let%expect_test "compile_header_row sample" =
  let header = (50, 98, 2) in
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
  |>| function
  | _ :: table ->
      Path.compile_header_row table header
      |>| Option.map Path.show
      |>| Option.map print_endline
      |>| ignore;
      [%expect {| { Path.window = (98, 98); offset = -31 } |}]
  | _ -> failwith "no table"

let%expect_test "compile_header_row seeds" =
  let header = (1903578414, 0, 20266514) in
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
  |>| function
  | _ :: table ->
      Path.compile_header_row table header
      |>| Option.map Path.show
      |>| Option.map print_endline
      |>| ignore;
      [%expect
        {|
           { Path.window = (0, 20266513); offset = 1545059330 } |}]
  | _ -> failwith "no table"

let%expect_test "compile_table sample" =
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Path.compile_table
  |>| Path.show_paths
  |>| print_endline;
  [%expect {| [{ Path.window = (98, 98); offset = -31 }] |}]

let%expect_test "compile_table seeds" =
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Path.compile_table
  |>| Path.show_paths
  |>| print_endline;
  [%expect
    {|
      [{ Path.window = (1239000360, 1308640991); offset = -487996698 };
        { Path.window = (3429737174, 3434135476); offset = -1565516394 };
        { Path.window = (1535118372, 1545830629); offset = 415864339 };
        { Path.window = (438168826, 446641972); offset = 2377399210 };
        { Path.window = (536917987, 544654657); offset = 316619784 };
        { Path.window = (3429676494, 3429737173); offset = -384049208 };
        { Path.window = (979257605, 990438940); offset = 1318336963 };
        { Path.window = (3401349677, 3404859217); offset = -1031482541 };
        { Path.window = (2086137846, 2088091502); offset = -847138651 };
        { Path.window = (2683163217, 2692186170); offset = 1533326409 };
        { Path.window = (1633120598, 1662888272); offset = -1331275069 };
        { Path.window = (2940939059, 2946419136); offset = 104748907 };
        { Path.window = (498356425, 515916487); offset = 573016119 };
        { Path.window = (2711764761, 2724264424); offset = 1290124945 };
        { Path.window = (2605613670, 2623158994); offset = -2420710039 };
        { Path.window = (4294380836, 4294967295); offset = -308853639 };
        { Path.window = (3169731719, 3170733408); offset = -2810516698 };
        { Path.window = (1124514126, 1129263946); offset = 1436858815 };
        { Path.window = (773812762, 835303388); offset = 3316722590 };
        { Path.window = (20266514, 26921881); offset = 1306626526 };
        { Path.window = (1808537666, 1814135908); offset = 2276399443 };
        { Path.window = (130988760, 144724434); offset = 1523978333 };
        { Path.window = (1098613331, 1114497264); offset = 2634856385 };
        { Path.window = (858089779, 873640810); offset = 2695959987 };
        { Path.window = (2361908616, 2383690007); offset = -1747071946 };
        { Path.window = (4237225295, 4240432415); offset = -251111638 };
        { Path.window = (2253415952, 2269053206); offset = -2050966996 };
        { Path.window = (3832436687, 3847103899); offset = -1963817604 };
        { Path.window = (650502764, 657139062); offset = 2129081357 };
        { Path.window = (3970575741, 3995948346); offset = -3752489530 };
        { Path.window = (2568591486, 2604369519); offset = -1223292607 };
        { Path.window = (768686733, 773812761); offset = 2341509609 };
        { Path.window = (0, 20266513); offset = 1545059330 };
        { Path.window = (3180154163, 3192666657); offset = -1795533993 };
        { Path.window = (2776195129, 2779175480); offset = -409308345 };
        { Path.window = (1548974917, 1557270806); offset = -215426509 };
        { Path.window = (4119971124, 4145767629); offset = -2848556938 };
        { Path.window = (929849138, 957158779); offset = 3127778329 };
        { Path.window = (2667969937, 2681059372); offset = -1427017085 };
        { Path.window = (1816346942, 1895435163); offset = 797624495 };
        { Path.window = (2383690008, 2416384373); offset = 959744588 }] |}]

let%test "compiled sample table is correct" =
  let _, sections = Parser.parse_almanac Parser_test.seeds_sample_text in
  Parser_test.seeds_sample_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Path.compile_table
  |>| List.for_all (fun path ->
          let Path.{ window; offset } = path in
          let seeds = Range.to_list window in
          let expected =
            List.map (fun seed -> Look.look_table seed sections) seeds
          in
          let results = List.map (fun seed -> seed + offset) seeds in
          expected = results)

let%test "compiled seed table is correct" =
  let _, sections = Parser.parse_almanac Parser_test.seeds_text in
  Parser_test.seeds_text
  |>| Parser.parse_almanac
  |>| snd
  |>| Path.compile_table
  |>| List.for_all (fun path ->
          let Path.{ window; offset } = path in
          let x, z = window in
          let seeds = [ x + 1; x + 2; z ] in
          let expected =
            List.map (fun seed -> Look.look_table seed sections) seeds
          in
          let results = List.map (fun seed -> seed + offset) seeds in
          expected = results)
(*
     let%expect_test "compiled seed table is correct" =
       let _, sections = Parser.parse_almanac Parser_test.seeds_sample_text in
       Parser_test.seeds_text
       |>| Parser.parse_almanac
       |>| snd
       |>| Path.compile_table
       |>| List.iter (fun path ->
               let Path.{ window; offset } = path in
               let x, _ = window in
               let seeds = [ x ] in
               List.iter
                 (fun seed ->
                   let left = look_table seed sections
                   and right = seed + offset in
                   print_endline (Format.sprintf "%d -> %d = %d ?" seed left right))
                 seeds);
       [%expect {||}] *)
(*
     let%expect_test "compiled seed table" =
       let _, sections = Parser.parse_almanac Parser_test.seeds_text in
       Parser_test.seeds_text
       |>| Parser.parse_almanac
       |>| snd
       |>| Path.compile_table
       |>| List.iter (fun path ->
               let Path.{ window; offset } = path in
               let x, _y = window in
               let seeds = [ x + 1 ] in
               List.iter
                 (fun seed ->
                   let left = look_table seed sections
                   and right = seed + offset in
                   print_endline (Format.sprintf "%d -> %d = %d ?" seed left right))
                 seeds);
       [%expect {||}] *)
