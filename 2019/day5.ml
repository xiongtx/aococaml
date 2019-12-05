open Core

(** Part 1 **)

let codes =
  "./resources/day5.txt"
  |> Filename.realpath
  |> In_channel.read_all
  |> String.rstrip
  |> String.split ~on:','
  |> List.map ~f:int_of_string

let rec exec_codes arr pos output =
  let instruction = sprintf "%05d" arr.(pos) in
  let opcode = String.slice instruction (-2) 0 in
  let p1_mode = String.get instruction 2 in
  let p2_mode = String.get instruction 1 in
  match opcode with
  | "01" | "02" -> let v1 = if p1_mode = '1' then arr.(pos + 1)
                           else arr.(arr.(pos + 1)) in
                  let v2 = if p2_mode = '1' then arr.(pos + 2)
                           else arr.(arr.(pos + 2)) in
                  let total = if opcode = "01" then v1 + v2
                              else v1 * v2 in
                  arr.(arr.(pos + 3)) <- total;
                  exec_codes arr (pos + 4) output
  | "03" -> arr.(arr.(pos + 1)) <- 1;
           exec_codes arr (pos + 2) output
  | "04" -> let v1 = if p1_mode = '1' then arr.(pos + 1)
                    else arr.(arr.(pos + 1)) in
           exec_codes arr (pos + 2) (v1 :: output)
  | "99" -> (arr, output)
  | s -> failwith (sprintf "Invalid instruction: %s" s)

let execute_codes codes =
  let arr = Array.of_list codes in
  let (_, output) = (exec_codes arr 0 []) in
  List.nth_exn output 0

let () = printf "Part 1: %d\n" (execute_codes codes)

(** Part 2 **)

let rec exec_more_codes arr pos output =
  let instruction = sprintf "%05d" arr.(pos) in
  let opcode = String.slice instruction (-2) 0 in
  let p1_mode = String.get instruction 2 in
  let p2_mode = String.get instruction 1 in
  match opcode with
  | "01" | "02" | "05" | "06" | "07" | "08" -> begin
      let v1 = if p1_mode = '1' then arr.(pos + 1)
               else arr.(arr.(pos + 1)) in
      let v2 = if p2_mode = '1' then arr.(pos + 2)
               else arr.(arr.(pos + 2)) in
      match opcode with
      | "01" -> arr.(arr.(pos + 3)) <- v1 + v2;
               exec_more_codes arr (pos + 4) output
      | "02" -> arr.(arr.(pos + 3)) <- v1 * v2;
               exec_more_codes arr (pos + 4) output
      | "05" -> let new_pos = if v1 <> 0 then v2
                             else pos + 3 in
               exec_more_codes arr new_pos output
      | "06" -> let new_pos = if v1 = 0 then v2
                             else pos + 3 in
               exec_more_codes arr new_pos output
      | "07" -> let store = if v1 < v2 then 1
                           else 0 in
               arr.(arr.(pos + 3)) <- store;
               exec_more_codes arr (pos + 4) output
      | "08" -> let store = if v1 = v2 then 1
                           else 0 in
               arr.(arr.(pos + 3)) <- store;
               exec_more_codes arr (pos + 4) output
      | _ -> failwith "Impossible"
    end
  | "03" -> arr.(arr.(pos + 1)) <- 5;
           exec_more_codes arr (pos + 2) output
  | "04" -> let v1 = if p1_mode = '1' then arr.(pos + 1)
                    else arr.(arr.(pos + 1)) in
           exec_more_codes arr (pos + 2) (v1 :: output)
  | "99" -> (arr, output)
  | s -> failwith (sprintf "Invalid instruction: %s" s)

let execute_more_codes codes =
  let arr = Array.of_list codes in
  let (_, output) = (exec_more_codes arr 0 []) in
  List.nth_exn output 0

let () = printf "Part 2: %d\n" (execute_more_codes codes)
