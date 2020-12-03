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

  let slop = 3

  let explode s = List.init end_offset ~f:(String.get s)

  let on_tree offset line : bool =
    let cl = explode line in
    (* print_list cl; *)
    let c = List.nth_exn cl offset in
    Char.( = ) c tree

  let count_offset old = (old + slop) % end_offset

  let test_line (offset, count) line =
    (* printf "Offset: %d, count %d " offset count; *)
    let new_offset = count_offset offset in
    if on_tree offset line then (new_offset, count + 1) else (new_offset, count)

  let part1 input =
    let lines = read input in
    let _, count = List.fold ~init:(start_offset, 0) ~f:test_line lines in
    printf "Result: %d\n " count
end

let () = Day3.part1 "./input"
