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

val parse_input : string -> machine
val initial_state : machine -> machine_state
