open Base
open Stdio

module Day2 = struct
  type t = { l : string; m : string; c : string; password : string }

  let splitline line =
    (* printf "%s\n" line; *)
    match String.split line ~on:':' with
    | [] -> None
    | [ policy; password ] -> (
        match String.split ~on:' ' policy with
        | [] -> None
        | [ lm; c ] -> (
            match String.split ~on:'-' lm with
            | [] -> None
            | [ l; m ] -> Some { l; m; c; password }
            | _ -> None )
        | _ -> None )
    | _ -> None

  let count_char (c : string) (password : string) =
    String.fold ~init:0
      ~f:(fun acc sc -> if Char.equal c.[0] sc then acc + 1 else acc)
      password

  let getentry e = Option.value_exn e

  let isOkForPolicy (e : t option) : bool =
    let entry = getentry e in
    let occ = count_char entry.c entry.password in
    let min = Int.of_string entry.l in
    let max = Int.of_string entry.m in
    (* printf "%d <= %d <= %d\n" min occ max; *)
    occ >= min && occ <= max

  let part1 input =
    let count =
      List.length
        (List.filter
           ~f:(fun ret -> Bool.( = ) ret true)
           (List.map ~f:isOkForPolicy (List.map ~f:splitline input)))
    in
    printf "Count: %d\n" count
end

let () = Day2.part1 Inputdata.input
