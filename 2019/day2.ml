open Core

(** Part 1 **)

let codes =
  "./resources/day2.txt"
  |> Filename.realpath
  |> In_channel.read_all
  |> String.rstrip
  |> String.split ~on:','
  |> List.map ~f:int_of_string

let rec exec_codes arr pos =
  let instruction = arr.(pos) in
  match instruction with
  | 1 -> arr.(arr.(pos + 3)) <- arr.(arr.(pos + 1)) + arr.(arr.(pos + 2));
         exec_codes arr (pos + 4)
  | 2 -> arr.(arr.(pos + 3)) <- arr.(arr.(pos + 1)) * arr.(arr.(pos + 2));
         exec_codes arr (pos + 4)
  | 99 -> arr
  | n -> failwith (sprintf "Invalid instruction: %d" n)

let execute_codes codes =
  let arr = Array.of_list codes in
  arr.(1) <- 12;
  arr.(2) <- 2;
  (exec_codes arr 0).(0)

let () = printf "Part 1: %d\n" (execute_codes codes)

(** Part 2 **)

let rec find_pair' codes pairs =
  let goal = 19690720 in
  match pairs with
  | [] -> failwith "No valid noun / verb pairs"
  | (noun, verb) as pair :: rest -> let arr = Array.of_list codes in
                                    arr.(1) <- noun;
                                    arr.(2) <- verb;
                                    if (exec_codes arr 0).(0) = goal then
                                      pair
                                    else find_pair' codes rest

let find_pair codes =
  let range = List.range 0 100 in
  let pairs = range
              |> List.map ~f:(fun x -> List.map range ~f:(fun y -> (x, y)))
              |> List.concat in
  find_pair' codes pairs

let part_2 codes =
  let (noun, verb) = find_pair codes in
  100 * noun + verb

let () = printf "Part 2: %d\n" (part_2 codes)
