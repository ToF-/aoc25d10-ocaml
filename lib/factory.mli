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

val string_of_int_list : int list -> string
val string_of_int_list_list : int list list -> string
val string_of_int_array : int array -> string
val parse_input : string -> machine
val initial_state : machine -> machine_state
val push_button : machine_state -> int list -> machine_state
val shortest_sequence_to_diagram : machine -> int list list
