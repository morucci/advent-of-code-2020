(* open Base *)
open Stdio
open Base

let rec print_list = function
  | [] -> ()
  | e :: l ->
      printf "%d" e;
      printf " ";
      print_list l

module Day1 = struct
  let is2020 (x : int) (xs : int list) : int =
    let filtered =
      List.filter
        ~f:(fun (_, res) -> res = 2020)
        (List.map ~f:(fun s -> (s, x + s)) xs)
    in
    match List.hd filtered with Some (n, _) -> n | None -> -1

  let is2020_3 (x : int) (y : int) (xs : int list) : int = is2020 (x + y) xs

  let rec solve (input : int list) : int * int =
    match input with
    | [] -> (0, 0)
    | x :: xs ->
        let ret = is2020 x xs in
        if ret > -1 then (x, ret) else solve xs

  let rec _solve2 (input : int list) =
    match input with
    | [] -> (0, 0, 0)
    | [ _ ] -> (0, 0, 0)
    | [ _; _ ] -> (0, 0, 0)
    | [ _; _; _ ] -> (0, 0, 0)
    | x :: y :: xs ->
        let ret = is2020_3 x y xs in
        if ret > -1 then (x, y, ret) else _solve2 (x :: xs)

  let rec solve2 (input : int list) =
    match input with
    | [] -> (0, 0, 0)
    | [ _ ] -> (0, 0, 0)
    | [ _; _ ] -> (0, 0, 0)
    | [ _; _; _ ] -> (0, 0, 0)
    | x :: y :: xs ->
        let a, b, c = _solve2 (x :: y :: xs) in
        if a > 0 then (a, b, c) else solve2 (y :: xs)
end

let part1 () =
  let x, n = Day1.solve Inputdata.input in
  printf "%d\n" (x * n)

let part2 () =
  let x, y, z = Day1.solve2 Inputdata.input in
  printf "%d\n" (x * y * z)

let () = part2 ()
