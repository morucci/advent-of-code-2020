(* open Stdio *)
open Core

(* let print_list (a : string list list) =
  List.iter ~f:(fun l -> printf "%s\n" (String.concat l ~sep:" ")) a;
  printf "\n" *)
let print_list (a : string list) = printf "%s\n" (String.concat a ~sep:" ")

let read file = In_channel.read_lines file

let tlc s = List.init (String.length s) ~f:(String.get s)

let cl2s cl = String.concat (List.map ~f:(String.make 1) cl)

exception Invalid_input

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

  let get_key_value (rk : string) (pkvs : string list list) =
    let filtered =
      List.filter
        ~f:(fun l ->
          match l with [] -> raise Invalid_input | k :: _ -> String.( = ) rk k)
        pkvs
    in
    List.hd_exn (List.rev (List.hd_exn filtered))

  let check_byr v =
    let v_int = int_of_string v in
    String.length v = 4 && v_int >= 1920 && v_int <= 2002

  let check_iyr v =
    let v_int = int_of_string v in
    String.length v = 4 && v_int >= 2010 && v_int <= 2020

  let check_eyr v =
    let v_int = int_of_string v in
    String.length v = 4 && v_int >= 2020 && v_int <= 2030

  let check_hgt v =
    let check_cm v =
      let v_int = int_of_string v in
      v_int >= 150 && v_int <= 193
    in
    let check_in v =
      let v_int = int_of_string v in
      v_int >= 59 && v_int <= 76
    in
    let chars = tlc v in
    match List.rev chars with
    | [] -> false
    | x :: y :: xs -> (
        match (x, y) with
        | 'm', 'c' -> check_cm (cl2s (List.rev xs))
        | 'n', 'i' -> check_in (cl2s (List.rev xs))
        | _ -> false )
    | _ -> false

  let check_hcl v =
    let check_char c =
      Char.between ~low:'0' ~high:'9' c || Char.between ~low:'a' ~high:'f' c
    in
    let check_inner lc =
      List.length
        (List.filter ~f:(Bool.( = ) false) (List.map ~f:check_char lc))
      = 0
    in
    let chars = tlc v in
    match chars with
    | [] -> false
    | x :: xs -> if Char.( = ) x '#' then check_inner xs else false

  let check_ecl v =
    match v with
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

  let check_pid v = String.length v = 9

  let rules_checker (rk : string) (pkvs : string list list) =
    let v = get_key_value rk pkvs in
    match rk with
    | "byr" -> check_byr v
    | "iyr" -> check_iyr v
    | "eyr" -> check_eyr v
    | "hgt" -> check_hgt v
    | "hcl" -> check_hcl v
    | "ecl" -> check_ecl v
    | "pid" -> check_pid v
    | _ -> true

  let verify_passport (raw : string list) check_rules : bool =
    let ref_keys = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid" ] in
    let pkvs = List.map ~f:(fun kv -> String.split ~on:':' kv) raw in
    let present rk =
      List.mem (List.map ~f:List.hd_exn pkvs) rk ~equal:String.( = )
    in
    let validate rk =
      match rk with
      | "cid" -> true
      | _ as rk' -> present rk && check_rules rk' pkvs
    in
    if
      List.length
        (List.filter ~f:(Bool.( = ) false) (List.map ~f:validate ref_keys))
      = 0
    then true
    else false

  let traverse input check_rules =
    let lines = read input in
    let acc = List.fold ~init:[ [] ] ~f:process_line lines in
    let validity_list =
      List.map ~f:(fun passport -> verify_passport passport check_rules) acc
    in
    List.filter ~f:(Bool.( = ) true) validity_list

  let part1 input =
    let count = traverse input (fun _ _ -> true) in
    printf "Result part 1: %d\n " (List.length count)

  let part2 input =
    let count = traverse input rules_checker in
    printf "Result part 1: %d\n " (List.length count)
end

(* let () = Day4.part1 "./input" *)
let () = Day4.part2 "./input"
