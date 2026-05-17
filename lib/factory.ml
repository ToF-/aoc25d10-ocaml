open Printf

let string_of_int_list l =
  String.concat "" [ "["; String.concat "; " (List.map string_of_int l); "]" ]

let string_of_int_list_list l =
  String.concat ""
    [ "["; String.concat ";" (List.map string_of_int_list l); "]" ]

(* [|[|0; 0; 0; 0|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|] *)

let string_of_int_array a =
  String.concat ""
    [
      "[|"; String.concat ";" (Array.to_list (Array.map string_of_int a)); "|]";
    ]

let string_of_int_array_array a =
  String.concat ""
    [
      "[|";
      String.concat ";" (List.map string_of_int_array (Array.to_list a));
      "|]";
    ]

type machine = {
  diagram : int array;
  buttons : int list list;
  joltage : int array;
}

type machine_state = {
  diagram : int array;
  sequence : int list list;
  joltage : int array;
}

let parse_diagram s =
  let trimmed = String.sub s 1 (String.length s - 2) in
  Array.init (String.length trimmed) (fun i ->
      match String.get trimmed i with '#' -> 1 | _ -> 0)

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

let initial_state (machine : machine) =
  {
    diagram = Array.init (Array.length machine.diagram) (fun _ -> 0);
    sequence = [];
    joltage = Array.init (Array.length machine.joltage) (fun _ -> 0);
  }

let push_button (state : machine_state) (button : int list) =
  let diagram = Array.copy state.diagram in
  let joltage = Array.copy state.joltage in
  button
  |> List.iter (fun switch ->
      joltage.(switch) <- joltage.(switch) + 1;
      diagram.(switch) <- joltage.(switch) mod 2);
  { diagram; sequence = List.cons button state.sequence; joltage }

module StateOrder = struct
  type t = machine_state

  let compare a b =
    Stdlib.compare (a.sequence |> List.length) (b.sequence |> List.length)
end

module StateQueue = Pqueue.MakeMin (StateOrder)

module IntArrayOrder = struct
  type t = int array

  let compare = Stdlib.compare
end

module IntArraySet = Set.Make (IntArrayOrder)

let print_state state =
  printf "%s " (string_of_int_array state.diagram);
  printf "%s " (string_of_int_list_list state.sequence);
  printf "%s\n" (string_of_int_array state.joltage)

let print_queue queue =
  let queue' = queue |> StateQueue.copy in
  if queue' |> StateQueue.is_empty then printf "empty queue"
  else
    while not (queue' |> StateQueue.is_empty) do
      let state = queue' |> StateQueue.get_min_elt in
      print_state state;
      queue' |> StateQueue.remove_min
    done;
  printf "\n"

let print_set set =
  if set |> IntArraySet.is_empty then printf "empty set"
  else
    set |> IntArraySet.elements
    |> List.iter (fun a -> printf "%s" (string_of_int_array a))

let shortest_sequence_to_diagram (machine : machine) =
  let target = machine.diagram in
  let initial = initial_state machine in
  let to_visit = StateQueue.create () in
  let rec visit visited =
    let state_opt = to_visit |> StateQueue.pop_min in
    match state_opt with
    | None -> invalid_arg "impossible to find target"
    | Some state ->
        if state.diagram = target then state.sequence
        else (
            let visited' = visited |> IntArraySet.add state.diagram in
          machine.buttons
          |> List.iter (fun button ->
              let state' = push_button state button in
              StateQueue.add to_visit state');
          visit visited')
  in
  StateQueue.add to_visit initial;
  visit (IntArraySet.empty)
