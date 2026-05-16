type machine = { diagram : int; buttons : int list list; joltage : int list }
type sequence = machine

val parse_input : string -> machine
val matrix_of_machine : machine -> int array array
val matrix_reduce : int array array -> int array array
val push : sequence -> int list -> sequence
