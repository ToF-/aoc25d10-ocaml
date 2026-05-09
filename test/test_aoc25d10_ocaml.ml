open OUnit2
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

let parse_input = Aoc25d10_ocaml.Factory.parse_input
let matrix_of_machine = Aoc25d10_ocaml.Factory.matrix_of_machine
let input_line_example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"

let tests =
  "factory"
  >::: [
         ( "parsing input line" >:: fun _ ->
           let machine = parse_input input_line_example in
           assert_equal ~printer:string_of_int_list [ 3; 5; 4; 7 ]
             machine.joltage;
           assert_equal ~printer:string_of_int_list_list
             [ [ 3 ]; [ 1; 3 ]; [ 2 ]; [ 2; 3 ]; [ 0; 2 ]; [ 0; 1 ] ]
             machine.buttons );
         ( "making a matrix from input" >:: fun _ ->
           let machine = parse_input input_line_example in
           let matrix = matrix_of_machine machine in
           let expected =
             [|
               [| 0; 0; 0; 0; 1; 1; 3 |];
               [| 0; 1; 0; 0; 0; 1; 5 |];
               [| 0; 0; 1; 1; 1; 0; 4 |];
               [| 1; 1; 0; 1; 0; 0; 7 |];
             |]
           in
           assert_equal ~printer:string_of_int_array_array expected matrix );
       ]

let _ = run_test_tt_main tests
