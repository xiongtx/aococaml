open Core

(** Part 1 **)

let lower :: [upper] =
  "./resources/day4.txt"
  |> Filename.realpath
  |> In_channel.read_all
  |> String.rstrip
  |> String.split ~on:'-'
  |> List.map ~f:int_of_string

let has_adjacent_pair l =
  l
  |> List.fold ~init:(false, ' ') ~f:(fun (tf, c) new_c ->
         if tf then (true, new_c) else (c = new_c, new_c))
  |> (fun (tf, _) -> tf)

let is_monotonic_increase l = (List.sort ~compare:Char.compare l) = l

let meets_criteria n =
  let l = n |> Int.to_string |> String.to_list in
  has_adjacent_pair l && is_monotonic_increase l

let number_meeting_criteria lower upper =
  List.range lower (upper + 1)
  |> List.count ~f:meets_criteria

let () = printf "Part 1: %d\n" (number_meeting_criteria lower upper)

(** Part 2 **)

let partition l =
  l
  |> List.fold ~init:([], []) ~f:(fun (all, current) e ->
         match current with
         | [] -> (all, [e])
         | e :: _ -> if x = e then (all, (e :: current))
                     else (current :: all, [e]))
  |> (fun (all, current) -> current :: all)
  |> List.rev

let has_exact_pair l =
  l
  |> partition
  |> List.exists ~f:(fun l -> List.length l = 2)

let meets_new_criteria n =
  let l = n |> Int.to_string |> String.to_list in
  has_exact_pair l && is_monotonic_increase l

let number_meeting_new_criteria lower upper =
  List.range lower (upper + 1)
  |> List.count ~f:meets_new_criteria

let () = printf "Part 2: %d\n" (number_meeting_new_criteria lower upper)
