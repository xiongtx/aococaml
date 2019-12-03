open Core

(** Part 1 **)

type step = Up of int | Right of int | Down of int | Left of int

type position = {x: int; y: int}

let parse_step s =
  let dir = String.get s 0 in
  let distance = String.slice s 1 0 |> int_of_string in
  match dir with
  | 'U' -> Up distance
  | 'R' -> Right distance
  | 'D' -> Down distance
  | 'L' -> Left distance
  | c -> failwith (sprintf "Invalid step: %c" c)

let first_steps :: [second_steps] =
  "./resources/day3.txt"
  |> Filename.realpath
  |> In_channel.read_lines
  |> List.map ~f:(fun s -> s
                           |> String.split ~on:','
                           |> List.map ~f:parse_step)

let move (pos: position) (step: step) =
  let {x; y} = match step with
    | Left n -> {x = -n; y = 0}
    | Right n -> {x = n; y = 0}
    | Up n -> {x = 0; y = n}
    | Down n -> {x = 0; y = -n} in
  {x = pos.x + x; y = pos.y + y}

type segment = {p: position; q: position}

type orientation = Clockwise | Counterclockwise | Collinear

let orientation {x = x1; y = y1} {x = x2; y = y2} {x = x3; y = y3} =
  match Int.sign ((y2 - y1)*(x3 - x2) - (y3 - y2)*(x2 - x1)) with
  | Neg -> Counterclockwise
  | Zero -> Collinear
  | Pos -> Clockwise

let intersection {p = p1; q = q1} {p = p2; q = q2} =
  let o1 = orientation p1 q1 p2 in
  let o2 = orientation p1 q1 q2 in
  let o3 = orientation p2 q2 p1 in
  let o4 = orientation p2 q2 q1 in
  if p1 <> p2 && o1 <> o2 && o3 <> o4 then
    let pos = if p1.y = q1.y then
                {x = p2.x; y = p1.y}
              else
                {x = p1.x; y = p2.y}
    in Some pos
  else None

let manhattan_distance {x = x1; y = y1} {x = x2; y = y2} =
  abs (x1 - x2) + abs (y1 - y2)

let segments steps =
  steps
  |> List.fold ~init:([], {x = 0; y = 0}) ~f:(fun (l, pos) step ->
         let new_pos = move pos step in
         ({p = pos; q = new_pos} :: l, new_pos))
  |> (fun (l, _) -> l)
  |> List.rev

let intersections first_steps second_steps =
  let first_segments = segments first_steps in
  let second_segments = segments second_steps in
  List.cartesian_product first_segments second_segments
  |> List.filter_map ~f:(fun (s1, s2) -> intersection s1 s2)

let closest_distance first_steps second_steps =
  intersections first_steps second_steps
  |> List.min_elt ~compare:(fun p q ->
         let origin = {x = 0; y = 0} in
         (manhattan_distance p origin) - (manhattan_distance q origin))
  |> (function | Some pos -> manhattan_distance pos {x = 0; y = 0}
               | None -> failwith "No intersection")

let () = printf "Part 1: %d\n" (closest_distance first_steps second_steps)

(** Part 2 **)

type segment_dist = {u: position; v: position; d: int}

let segment_dists steps =
  steps
  |> List.fold ~init:([], {x = 0; y = 0}, 0) ~f:(fun (l, pos, d) step ->
         let new_pos = move pos step in
         let new_d = (manhattan_distance pos new_pos) + d in
         ({u = pos; v = new_pos; d = new_d} :: l, new_pos, new_d))
  |> (fun (l, _, _) -> l)
  |> List.rev

let intersection_dist {u = u1; v = v1; d = d1} {u = u2; v = v2; d = d2} =
  let o1 = orientation u1 v1 u2 in
  let o2 = orientation u1 v1 v2 in
  let o3 = orientation u2 v2 u1 in
  let o4 = orientation u2 v2 v1 in
  if u1 <> u2 && o1 <> o2 && o3 <> o4 then
    let pos, d = if u1.y = v1.y then
                   let d = (d1 - (abs (v1.x - u2.x))) + (d2 - (abs (v2.y - u1.y))) in
                   ({x = u2.x; y = u1.y;}, d)
                 else
                   let d = (d1 - (abs (v1.y - u2.y))) + (d2 - (abs (v2.x - u1.x))) in
                   ({x = u1.x; y = u2.y}, d)
    in Some (pos, d)
  else None

let intersection_dists first_steps second_steps =
  let first_segment_dists = segment_dists first_steps in
  let second_segment_dists = segment_dists second_steps in
  List.cartesian_product first_segment_dists second_segment_dists
  |> List.filter_map ~f:(fun (s1, s2) -> intersection_dist s1 s2)

let fewest_combined_steps first_steps second_steps =
  intersection_dists first_steps second_steps
  |> List.min_elt ~compare:(fun (_, d1) (_, d2) -> d1 - d2)
  |> (function | Some (_, d) -> d
               | None -> failwith "No intersection")

let () = printf "Part 2: %d\n" (fewest_combined_steps first_steps second_steps)
