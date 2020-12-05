(* open Stdio *)
open Core

let print_list (a : string list) = printf "%s\n" (String.concat a ~sep:" ")

let read file = In_channel.read_lines file

module Day4 = struct
  type seat = { col : string; row : string }

  let pow n p =
    let fn = float_of_int n in
    let fp = float_of_int p in
    int_of_float (fn ** fp)

  let bitstring_to_int s =
    String.foldi ~init:0 (String.rev s) ~f:(fun i acc c ->
        acc + (int_of_string (Char.to_string c) * pow 2 i))

  let process_line acc line =
    let to_bin =
      String.fold ~init:{ col = ""; row = "" }
        ~f:(fun acc c ->
          match c with
          | 'F' -> { row = acc.row ^ Char.escaped '0'; col = acc.col }
          | 'B' -> { row = acc.row ^ Char.escaped '1'; col = acc.col }
          | 'L' -> { row = acc.row; col = acc.col ^ Char.escaped '0' }
          | 'R' -> { row = acc.row; col = acc.col ^ Char.escaped '1' }
          | _ as inv -> raise (Invalid_argument (Char.escaped inv)))
        line
    in
    printf "%s\n" line;
    printf "row: %s, col: %s \n" to_bin.row to_bin.col;
    printf "row: %d, col: %d \n"
      (bitstring_to_int to_bin.row)
      (bitstring_to_int to_bin.col);
    let ret = (bitstring_to_int to_bin.row * 8) + bitstring_to_int to_bin.col in
    printf "Seat ID: %d\n" ret;
    if ret > acc then ret else acc

  let traverse input =
    let lines = read input in
    List.fold ~init:0 ~f:process_line lines

  let part1 input =
    let count = traverse input in
    printf "Result part 1: %d\n " count
end

let () = Day4.part1 "./input"
