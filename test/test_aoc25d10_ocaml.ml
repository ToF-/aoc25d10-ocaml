open OUnit2
open Printf

let string_of_int_list l =
    String.concat "" ["["; String.concat "; " (List.map string_of_int l); "]"]

let string_of_int_list_list l =
    String.concat "" [
        "[";
        String.concat ";" (List.map string_of_int_list l);
        "]"
    ]


let parse_input = Aoc25d10_ocaml.Factory.parse_input

let tests = "factory" >::: [
        ("parsing input line" >:: fun _ ->
            let input_line = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" in
            let machine = parse_input input_line in
            assert_equal ~printer:string_of_int_list [3; 5; 4; 7] (machine.joltage);
            assert_equal ~printer:string_of_int_list_list [[3];[1; 3];[2];[2; 3];[0; 2];[0; 1]] (machine.buttons) );
    ]
let _ = run_test_tt_main tests
