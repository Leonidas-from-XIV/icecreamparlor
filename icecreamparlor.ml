open Base
open Stdio

type t = {
  costs: (int * int) list;
  costs_array: (int * int) array;
  money: int;
}

let read_input () =
  let rev_compare (x, _index) (y, _index) =
    Int.compare y x
  in
  match In_channel.input_line In_channel.stdin with
  | Some line ->
    Int.of_string line
    |> List.init ~f:ignore
    |> List.map ~f:(fun () ->
        match In_channel.input_line In_channel.stdin, In_channel.input_line In_channel.stdin, In_channel.input_line In_channel.stdin with
        | Some money, Some array_size, Some costs ->
          let money = Int.of_string money in
          let len = Int.of_string array_size in
          let costs_array = Array.create ~len (0, 0) in
          let costs =
            costs
            |> String.split ~on:' '
            |> List.mapi ~f:(fun index v ->
              let price = Int.of_string v in
              costs_array.(index) <- (price, index);
              (price, index))
            |> List.sort ~compare:rev_compare
          in
          Array.sort costs_array ~compare:(fun (price, _) (price', _) -> Int.compare price price');
          { money; costs; costs_array }
        | _ -> failwith "Invalid format")
  | None -> failwith "No num"

let array_get array index =
  match array.(index) with
  | v -> Some v
  | exception Invalid_argument _ -> None

let pick_second
  : int -> int -> (int * int) array -> int option
  = fun already_used_index remaining costs_array ->
  match Array.binary_search costs_array ~compare:(fun (price, _) key -> Int.compare price key) `First_equal_to remaining with
  | None -> None
  | Some index ->
    match index = already_used_index with
    | false ->
      let (_, original_index) = costs_array.(index) in
      Some original_index
    | true ->
      match array_get costs_array (Int.succ index) with
      | Some (value, original_index) when value = remaining -> Some original_index
      | _ -> None

let rec pick money costs_array = function
  | [] -> None
  | (price, _) :: prices when price > money ->
    pick money costs_array prices
  | (price, index) :: prices ->
    let remaining = money - price in
    match pick_second index remaining costs_array with
    | None -> pick money costs_array prices
    | Some second_index ->
      match index < second_index with
      | true -> Some (index, second_index)
      | false -> Some (second_index, index)

let solve { money; costs; costs_array } =
  pick money costs_array costs

let print_solution = function
  | None -> print_endline "No solution"
  | Some (smaller, larger) -> Caml.Printf.printf "%d %d\n" (smaller + 1) (larger + 1)

let main () =
  read_input ()
  |> List.map ~f:solve
  |> List.iter ~f:print_solution

let () =
  main ()
