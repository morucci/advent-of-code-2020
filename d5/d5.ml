(* open Stdio *)
open Core

let print_list (a : string list) = printf "%s\n" (String.concat a ~sep:" ")

let read file = In_channel.read_lines file

let range a b = List.init (b - a) ~f:(( + ) a)

module Day4 = struct
  type seat = { col : string; row : string }

  let pow n p =
    let fn = float_of_int n in
    let fp = float_of_int p in
    int_of_float (fn ** fp)

  let bitstring_to_int s =
    String.foldi ~init:0 (String.rev s) ~f:(fun i acc c ->
        acc + (int_of_string (Char.to_string c) * pow 2 i))

  let process_line line =
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
    (bitstring_to_int to_bin.row * 8) + bitstring_to_int to_bin.col

  let get_max li =
    List.fold ~init:0 ~f:(fun acc i -> if i > acc then i else acc) li

  let traverse input =
    let lines = read input in
    List.map ~f:process_line lines

  let part1 input =
    let seats = traverse input in
    let sid = get_max seats in
    printf "Result part 1: %d\n " sid

  let part2 input =
    let seats = traverse input in
    let maxid = get_max seats in
    let minid = 0 in
    let is_in id l = List.mem ~equal:( = ) l id in
    let sid =
      List.hd_exn
        (List.filter
           ~f:(fun id ->
             (not (is_in id seats))
             && is_in (id - 1) seats
             && is_in (id + 1) seats)
           (range minid maxid))
    in

    printf "Result part 2: %d\n " sid
end

let () = Day4.part1 "./input"
let () = Day4.part2 "./input"
