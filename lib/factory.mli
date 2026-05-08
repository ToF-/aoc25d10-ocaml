type machine = { diagram : int; buttons : int list list; joltage : int list }

val parse_input : string -> machine
val make_matrix : machine -> int array array
