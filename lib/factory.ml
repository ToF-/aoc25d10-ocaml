open Printf

let read_line file_name =
  String.trim (In_channel.with_open_bin file_name In_channel.input_all)

let read_lines file_name =
  let lines =
    String.split_on_char '\n'
      (In_channel.with_open_bin file_name In_channel.input_all)
  in
  List.filter (fun s -> s <> "") lines

let string_of_int_list l =
  String.concat "" [ "["; String.concat "; " (List.map string_of_int l); "]" ]

let string_of_int_list_list l =
  String.concat ""
    [ "["; String.concat ";" (List.map string_of_int_list l); "]" ]

(* [|[|0; 0; 0; 0|]; [|0; 0; 0; 0|]; [|0; 0; 0; 0|]|] *)

let string_of_int_array a =
  String.concat ""
    [
      "[|"; String.concat ";" (Array.to_list (Array.map string_of_int a)); "|]\n";
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

let shortest_sequence_to_diagram ?(diagram = [||]) (machine : machine) =
  let target = if Array.length diagram > 0 then diagram else machine.diagram in
  let initial = initial_state machine in
  let to_visit = StateQueue.create () in
  let rec visit visited =
    let state_opt = to_visit |> StateQueue.pop_min in
    match state_opt with
    | None -> invalid_arg "impossible to find target"
    | Some state ->
        if state.diagram = target then state.sequence
        else
          let visited' = visited |> IntArraySet.add state.diagram in
          machine.buttons
          |> List.iter (fun button ->
              let state' = push_button state button in
              StateQueue.add to_visit state');
          visit visited'
  in
  StateQueue.add to_visit initial;
  visit IntArraySet.empty

let diagram_from_joltage (machine : machine) =
  let joltage = machine.joltage in
  Array.init (joltage |> Array.length) (fun i -> joltage.(i) mod 2)

let rec halves diagram =
  if not (diagram |> Array.for_all (fun v -> v mod 2 = 0)) then 1
  else (
    Array.map_inplace (fun v -> v / 2) diagram;
    2 * halves diagram)

let multiple joltage n =
    joltage |> Array.for_all (fun v -> v mod n = 0)


let shortest_sequence_length_to_joltage (machine : machine) =
  let diagram = diagram_from_joltage machine in
  let postlude = shortest_sequence_to_diagram ~diagram machine in
  printf "postlude: %s\n" (string_of_int_list_list postlude);
  let post_state : machine_state =
    List.fold_left
      (fun state button -> push_button state button)
      (initial_state machine) postlude
  in

  let target = machine.joltage |> Array.copy in
  target |> Array.mapi_inplace (fun i v -> v - post_state.joltage.(i));
  printf "target: %s\n" (string_of_int_array target);
  let repetitions = halves target in
  let to_visit = StateQueue.create () in
  let rec visit visited =
    let state_opt = to_visit |> StateQueue.pop_min in
    match state_opt with
    | None -> invalid_arg "impossible to reach target"
    | Some state ->
        print_state state;
        if state.joltage = target then state.sequence
        else
          let visited' = visited |> IntArraySet.add state.joltage in
          machine.buttons
          |> List.iter (fun button ->
              let state' = push_button state button in
              if Array.for_all2 (fun a b -> a <= b) state'.joltage target then
                StateQueue.add to_visit state');
          visit visited'
  in
  let initial = initial_state machine in
  StateQueue.add to_visit initial;
  printf "target joltage:%s\n" (string_of_int_array target);
  let prelude = visit IntArraySet.empty in
  (postlude |> List.length) + ((prelude |> List.length) * repetitions)

let solution_a file_name =
  let lines = read_lines file_name in
  lines
  |> List.fold_left
       (fun acc line ->
         let machine = parse_input line in
         let sequence = shortest_sequence_to_diagram machine in
         acc + List.length sequence)
       0

let matrix_of_machine (machine : machine) =
    let rows = machine.joltage |> Array.length in
    let cols = 1 + (machine.buttons |> List.length) in
    let matrix = Array.make_matrix rows cols 0 in
    machine.buttons |> List.iteri (fun col button ->
        button |> List.iter (fun row ->
            matrix.(col).(row) <- 1));
    machine.joltage |> Array.iteri (fun row value ->
        matrix.(row).(cols - 1) <- value);
        matrix

let solution_b file_name =
  let lines = read_lines file_name in
  lines
  |> List.fold_left
       (fun acc line ->
         let machine = parse_input line in
         let length = shortest_sequence_length_to_joltage machine in
         acc + length)
       0
