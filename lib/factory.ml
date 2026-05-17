open Printf

type machine = {
  diagram : int array;
  buttons : int list list;
  joltage : int array;
}

module IntListOrder = struct
  type t = int list

  let compare a b = Stdlib.compare (List.length a) (List.length b)
end

module IntListPQ = Pqueue.MakeMin (IntListOrder)
module IntSet = Set.Make (Int)

let parse_diagram s =
  Array.init (String.length s) (fun i ->
      match String.get s i with '#' -> 1 | _ -> 0)

let parse_int_list s =
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ',' |> List.map int_of_string

let parse_input input_line =
  let words = String.split_on_char ' ' input_line in
  let diagram = parse_diagram (List.hd words) in
  let buttons =
    List.map
      (fun s -> parse_int_list s)
      (List.drop 1 (List.take (List.length words - 1) words))
  in
  let joltage = Array.of_list (parse_int_list (List.hd (List.rev words))) in
  { diagram; buttons; joltage }
