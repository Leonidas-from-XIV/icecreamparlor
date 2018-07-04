open Base
open Stdio

type t = {
  costs: int list;
  money: int;
}

let read_input () =
  match In_channel.input_line In_channel.stdin with
  | Some line ->
    Int.of_string line
    |> List.init ~f:ignore
    |> List.map ~f:(fun () ->
        match In_channel.input_line In_channel.stdin, In_channel.input_line In_channel.stdin, In_channel.input_line In_channel.stdin with
        | Some money, Some _, Some costs ->
          let money = Int.of_string money in
          let costs = String.strip costs |> String.split ~on:' ' |> List.map ~f:Int.of_string in
          { money; costs; }
        | _ -> failwith "Invalid format")
  | None -> failwith "No num"

let pick_second
  : int -> (int * int) list -> int option
  = fun remaining prices ->
  List.Assoc.find prices ~equal:Int.equal remaining

let rec pick money = function
  | [] -> None
  | (price, _) :: prices when price > money ->
    pick money prices
  | (price, index) :: prices ->
    let remaining = money - price in
    match pick_second remaining prices with
    | None -> pick money prices
    | Some second_index ->
      match index < second_index with
      | true -> Some (index, second_index)
      | false -> Some (second_index, index)

let solve { money; costs } =
  let rev_compare (x, _index) (y, _index) =
    Int.compare y x
  in

  costs
  |> List.mapi ~f:(fun index e -> (e, index))
  |> List.sort ~compare:rev_compare
  |> pick money

let print_solution = function
  | None -> print_endline "No solution"
  | Some (smaller, larger) -> Caml.Printf.printf "%d %d\n" (smaller + 1) (larger + 1)

let main () =
  read_input ()
  |> List.map ~f:solve
  |> List.iter ~f:print_solution

let () =
  main ()
