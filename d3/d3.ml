(* open Stdio *)
open Core

let print_list (a : char list) =
  List.iter ~f:(printf "%c") a;
  printf "\n"

let read file = In_channel.read_lines file

module Day3 = struct
  let start_offset = 0

  let end_offset = 31

  let tree = '#'

  (* let rslop = 3

  let dslop = 1 *)

  let explode s = List.init end_offset ~f:(String.get s)

  let on_tree offset line : bool =
    let cl = explode line in
    (* print_list cl; *)
    let c = List.nth_exn cl offset in
    Char.( = ) c tree

  let count_offset old rslop = (old + rslop) % end_offset

  let test_line rslop dslop (offset, count, linen) line =
    (* printf "Offset: %d, count %d " offset count; *)
    match linen % dslop with
    | 1 -> (offset, count, linen + 1)
    | 0 ->
        let new_offset = count_offset offset rslop in
        if on_tree offset line then (new_offset, count + 1, linen + 1)
        else (new_offset, count, linen + 1)
    | _ -> (0, 0, 0)

  let traverse input rslop dslop =
    let test_line' = test_line rslop dslop in
    let lines = read input in
    List.fold ~init:(start_offset, 0, 0) ~f:test_line' lines

  let part1 input =
    let _, count, _ = traverse input 3 1 in
    printf "Result part 1: %d\n " count
  let part2 input =
    let _, c1, _ = traverse input 1 1 in
    let _, c2, _ = traverse input 3 1 in
    let _, c3, _ = traverse input 5 1 in
    let _, c4, _ = traverse input 7 1 in
    let _, c5, _ = traverse input 1 2 in
    printf "Result part 2: %d\n " (c1 * c2 * c3 * c4 * c5)
end

let () = 
  Day3.part1 "./input";
  Day3.part2 "./input"
