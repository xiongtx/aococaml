open Core

(** Part 1 **)

type step =
  | Right of int
  | Left of int

type direction = North | East | South | West

type position = {x: int; y: int}

let parse_step s =
  let n = String.length s in
  let num_steps = s
                  |> String.sub ~pos:1 ~len:(n - 1)
                  |> int_of_string in
  match String.get s 0 with
  | 'R' -> Right num_steps
  | 'L' -> Left num_steps

let steps =
  "./resources/day1.txt"
  |> Filename.realpath
  |> In_channel.read_all
  |> String.rstrip
  |> String.split_on_chars ~on:[' '; ',']
  |> List.filter ~f:(fun s -> s <> "")
  |> List.map ~f:parse_step

let new_direction_position (dir: direction) (pos: position) (step: step) =
  let new_dir, {x; y} = match dir, step with
    | North, Left n -> West, {x = -n; y = 0}
    | North, Right n -> East, {x = n; y = 0}
    | East, Left n -> North, {x = 0; y = n}
    | East, Right n -> South, {x = 0; y = -n}
    | South, Left n -> East, {x = n; y = 0}
    | South, Right n -> West, {x = -n; y = 0}
    | West, Left n -> South, {x = 0; y = -n}
    | West, Right n -> North, {x = 0; y = n} in
  new_dir, {x = pos.x + x; y = pos.y + y}

let distance steps =
  steps
  |> List.fold ~init:(North, {x = 0; y = 0}) ~f:(fun (dir, pos) step ->
         new_direction_position dir pos step)
  |> (fun (_, {x; y}) -> abs x + abs y)

let () = printf "Part 1: %d\n" (distance steps)

(** Part 2 **)

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
  if o1 <> o2 && o3 <> o4 then
    let pos = if p1.y = q1.y then
                {x = p2.x; y = p1.y}
              else
                {x = p1.x; y = p2.y}
    in Some pos
  else None

let point_distance {x = x1; y = y1} {x = x2; y = y2} =
  abs (x1 - x2) + abs (y1 - y2)

let first_intersection ({p = pos} as seg) segs =
  segs
  |> List.map ~f:(intersection seg)
  |> List.filter ~f:(function | Some p -> p <> pos | None -> false)
  |> List.min_elt ~compare:(fun (Some p) (Some q) ->
         point_distance pos p - point_distance pos q)
  |> (function | Some pos -> pos | None -> None)

let first_revisited_distance steps =
  steps
  |> List.fold ~init:(North, {x = 0; y = 0}, [], None) ~f:(fun (dir, pos, segments, pt) step ->
         if is_none pt then
           let (new_dir, new_pos) = new_direction_position dir pos step in
           let new_segment = {p = pos; q = new_pos} in
           let new_pt = first_intersection new_segment segments in
           (new_dir, new_pos, new_segment :: segments, new_pt)
         else (dir, pos, segments, pt))
  |> (fun (_, _, segments, opt) -> match opt with
                                   | Some {x; y} -> abs x + abs y
                                   | None -> 0)

let () = printf "Part 2: %d\n" (first_revisited_distance steps)
