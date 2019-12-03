open Core

(** Part 1 **)

let masses =
  "./resources/day1.txt"
  |> Filename.realpath
  |> In_channel.read_lines
  |> List.map ~f:float_of_string

let required_fuel mass =
  Float.round_down (mass /. 3.) -. 2.

let total_fuel masses =
  masses
  |> List.map ~f:required_fuel
  |> List.fold ~init:0. ~f:(+.)

let () = printf "Part 1: %f\n" (total_fuel masses)

(** Part 2 **)

let rec rec_fuel mass total_fuel =
  let fuel = required_fuel mass in
  if (fuel >. 0.) then
    rec_fuel fuel (total_fuel +. fuel)
  else total_fuel

let recursive_required_fuel mass = rec_fuel mass 0.

let total_recursive_fuel masses =
  masses
  |> List.map ~f:recursive_required_fuel
  |> List.fold ~init:0. ~f:(+.)

let () = printf "Part 2: %f\n" (total_recursive_fuel masses)
