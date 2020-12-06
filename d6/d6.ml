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
    let lines = In_channel.read_lines input in
    let acc = List.fold ~init:[ [] ] ~f:process_line lines in
    let z = List.map ~f:String.concat acc in
    let y = List.map ~f:keep_uniq_letter z in
    let r = List.map ~f:List.length y in
    let t = List.fold ~init:0 ~f:( + ) r in
    printf "Result: %d\n" t
end

let () = Day6.part1 "./input"
