open Core

let (|>) x f = f x

let instructions =
  "/home/txx/github/aococaml/2016/resources/day2.txt"
  |> In_channel.read_lines

exception Bad_instruction of char

type position = {x: int; y: int}

(* Part 1 *)

let execute_instruction ({x; y} as pos) ins =
  match ins with
  | 'U' -> {pos with y = y + (if y < 1 then 1 else 0)}
  | 'R' -> {pos with x = x + (if x < 1 then 1 else 0 )}
  | 'D' -> {pos with y = y - (if y > -1 then 1 else 0)}
  | 'L' -> {pos with x = x - (if x > -1 then 1 else 0)}
  | c -> raise (Bad_instruction c)

let position_to_button {x; y} =
  match (x, y) with
  | (-1, 1) -> "1"
  | (0, 1) -> "2"
  | (1, 1) -> "3"
  | (-1, 0) -> "4"
  | (0, 0) -> "5"
  | (1, 0) -> "6"
  | (-1, -1) -> "7"
  | (0, -1) -> "8"
  | (1, -1) -> "9"

let code instructions move_f translate_f =
  instructions
  |> List.fold ~init:({x = 0; y = 0}, "") ~f:(fun (pos, code) line ->
         let position = String.fold line ~init:pos ~f:move_f in
         (position, code ^ (translate_f position)))
  |> fun (_, code) -> code

let () = printf "Part 1: %s\n" (code instructions execute_instruction position_to_button)

let distance {x = x1; y = y1} {x = x2; y = y2} =
  abs x1 - x2 + abs y1 - y2

(* Part 2 *)

let execute_real_instruction ({x; y} as pos) ins =
  let origin = {x = 0; y = 0} in
  let new_pos = match ins with
    | 'U' -> {pos with y = y + 1}
    | 'R' -> {pos with x = x + 1}
    | 'D' -> {pos with y = y - 1}
    | 'L' -> {pos with x = x - 1}
    | c -> raise (Bad_instruction c) in
  if distance new_pos origin <= 2 then
    new_pos
  else pos

let real_position_to_button {x; y} =
  match (x, y) with
  | (0, 2) -> "1"
  | (-1, 1) -> "2"
  | (0, 1) -> "3"
  | (1, 1) -> "4"
  | (-2, 0) -> "5"
  | (-1, 0) -> "6"
  | (0, 0) -> "7"
  | (1, 0) -> "8"
  | (2, 0) -> "9"
  | (-1, -1) -> "A"
  | (0, -1) -> "B"
  | (1, -1) -> "C"
  | (0, -2) -> "D"

let () = printf "Part 2: %s\n" (code instructions execute_real_instruction real_position_to_button)
