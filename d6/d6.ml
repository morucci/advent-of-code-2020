(* open Stdio *)
open Core

(* let print_list (a : string list) = printf "%s\n" (String.concat a ~sep:" ") *)
let print_list_int (a : int list) = List.iter ~f:(fun e -> printf "%d" e) a

let explode s = List.init (String.length s) ~f:(String.get s)

let is_in_list e l = List.mem ~equal:Char.( = ) l e

let keep_uniq_letter str =
  let ex = explode str in
  List.fold ex ~init:[] ~f:(fun acc e ->
      if is_in_list e acc then acc else e :: acc)

let read file = In_channel.read_lines file

module Day6 = struct
  let process_line acc line =
    match String.length line = 0 with
    | true -> [] :: acc
    | false -> (
        match acc with
        | [] -> [ [ line ] ]
        | [ rp ] -> [ line :: rp ]
        | rp :: rest -> (line :: rp) :: rest )

  let part1 input =
    List.fold ~init:[ [] ] ~f:process_line (In_channel.read_lines input)
    |> List.map ~f:String.concat
    |> List.map ~f:keep_uniq_letter
    |> List.map ~f:List.length |> List.fold ~init:0 ~f:( + )
    |> printf "Result part1: %d\n"

  let all_c_yes_to_nq c l =
    let uniq = keep_uniq_letter l in
    let exp = explode l in
    List.fold uniq ~init:[] ~f:(fun acc e ->
        List.fold exp ~init:0 ~f:(fun acc' e' ->
            if Char.( = ) e' e then acc' + 1 else acc')
        :: acc)
    |> List.filter ~f:(fun c' -> c' = c)
    |> List.length

  let part2 input =
    List.fold ~init:[ [] ] ~f:process_line (In_channel.read_lines input)
    |> List.map ~f:(fun l -> (List.length l, l))
    |> List.map ~f:(fun (c, l) -> (c, String.concat l))
    |> List.map ~f:(fun (c, l) -> all_c_yes_to_nq c l)
    |> List.fold ~init:0 ~f:( + )
    |> printf "Result part2: %d\n"
end

let () =
  Day6.part1 "./input";
  Day6.part2 "./input"
