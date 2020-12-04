(* open Stdio *)
open Core

(* let print_list (a : string list list) =
  List.iter ~f:(fun l -> printf "%s\n" (String.concat l ~sep:" ")) a;
  printf "\n" *)
let print_list (a : string list) = printf "%s\n" (String.concat a ~sep:" ")

let read file = In_channel.read_lines file

module Day4 = struct
  let splitline rp line = List.append (String.split ~on:' ' line) rp

  let process_line acc line =
    match String.length line = 0 with
    | true -> [] :: acc
    | false -> (
        match acc with
        | [] -> [ splitline [] line ]
        | [ rp ] -> [ splitline rp line ]
        | rp :: rest -> splitline rp line :: rest )

  let verify_passport (raw : string list) : bool =
    let ref_keys = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid" ] in
    let pkeys =
      List.map ~f:(fun kv -> List.hd_exn (String.split ~on:':' kv)) raw
    in
    let inpassport rk =
      match rk with "cid" -> true | _ -> List.mem pkeys rk ~equal:String.( = )
    in
    if
      List.length
        (List.filter ~f:(Bool.( = ) false) (List.map ~f:inpassport ref_keys))
      = 0
    then true
    else false

  let traverse input =
    let lines = read input in
    let acc = List.fold ~init:[ [] ] ~f:process_line lines in
    let validity_list = List.map ~f:verify_passport acc in
    List.filter ~f:(Bool.( = ) true) validity_list

  let part1 input =
    let count = traverse input in
    printf "Result part 1: %d\n " (List.length count)
end

let () = Day4.part1 "./input"
