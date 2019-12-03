open Core

(** Part 1 **)

let triples =
  "./resources/day3.txt"
  |> Filename.realpath
  |> In_channel.read_lines
  |> List.map ~f:(fun s -> s
                           |> String.strip
                           |> String.split ~on:' '
                           |> List.filter ~f:(fun x -> x <> ""))
  |> List.map ~f:(fun l -> List.map l ~f:int_of_string)

let is_triangle l =
  let x :: y :: [z] = List.sort l ~compare:Int.compare in
  z < x + y

let num_triangles triples =
  triples
  |> List.count ~f:is_triangle

let () = printf "Part 1: %d\n" (num_triangles triples)

(** Part 2 **)

exception Invalid_triples_list

let rec vert_triples l =
  match l with
  | l1 :: l2 :: l3 :: lrest ->
     let x1 :: x2 :: [x3] = l1 in
     let y1 :: y2 :: [y3] = l2 in
     let z1 :: z2 :: [z3] = l3 in
     [x1; y1; z1] :: [x2; y2; z2] :: [x3; y3; z3] :: (vert_triples lrest)
  | [] -> []
  | _ -> raise Invalid_triples_list

let () =  printf "Part 2: %d\n" (num_triangles (vert_triples triples))
