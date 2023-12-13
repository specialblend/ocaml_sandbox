open Unix

let fill _ = Some "☕"
let drink _ = None

let rec handle cup =
  let cup =
    match cup with
    | Some coffee -> drink coffee
    | None -> fill "☕"
  in
  handle cup;
  sleep 1
