type machine = { diagram : int; buttons : int list list; joltage : int list }

val parse_input : string -> machine
val matrix_of_machine : machine -> int array array
