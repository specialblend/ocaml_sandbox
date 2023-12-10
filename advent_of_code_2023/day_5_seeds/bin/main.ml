open Fun
open Seeds

let _ =
  let table =
    "seeds.txt"
    |>| Core.In_channel.read_all
    |>| Parser.parse_almanac
    |>| fun (_, table) -> table
  in
  table
  |>| Pathfinder.compile_paths
  |>| List.iter
        begin
          fun path ->
            let Pathfinder.Path.{ domain; offset } = path in
            let verify n =
              let expected = Looker.look_table n table in
              let got = n + offset in
              assert (expected = got);
              print_endline (Printf.sprintf "%d-> %d" n got)
            in
            let x, y = domain in
            let _ = verify x in
            let _ = verify y in
            for n = x to y do
              verify n
            done
        end
