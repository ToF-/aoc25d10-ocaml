type machine = {
  diagram : int array;
  buttons : int list list;
  joltage : int array;
}

val parse_input : string -> machine
