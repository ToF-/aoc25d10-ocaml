open OUnit2
open Printf
open Aoc25d10_ocaml.Factory

let matrix_of_list l =
  let rows = l |> List.length in
  let cols = l |> List.hd |> List.length in
  let matrix = Array.make_matrix rows cols 0 in
  l
  |> List.iteri (fun row values ->
      values |> List.iteri (fun col value -> matrix.(row).(col) <- value));
  matrix

let input_line_example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
let machine = parse_input input_line_example

let tests =
  "factory"
  >::: [
         ( "parsing input line" >:: fun _ ->
           assert_equal ~printer:string_of_int_array [| 0; 1; 1; 0 |]
             machine.diagram;
           assert_equal ~printer:string_of_int_array [| 3; 5; 4; 7 |]
             machine.joltage;
           assert_equal ~printer:string_of_int_list_list
             [ [ 3 ]; [ 1; 3 ]; [ 2 ]; [ 2; 3 ]; [ 0; 2 ]; [ 0; 1 ] ]
             machine.buttons );
         ( "initial state of a machine" >:: fun _ ->
           let result = initial_state machine in
           assert_equal ~printer:string_of_int_list_list [] result.sequence;
           assert_equal ~printer:string_of_int_array [| 0; 0; 0; 0 |]
             result.diagram;
           assert_equal ~printer:string_of_int_array [| 0; 0; 0; 0 |]
             result.joltage );
         ( "pushing a button yields a new machine_state" >:: fun _ ->
           let initial = initial_state machine in
           let result = push_button (push_button initial [ 1; 3 ]) [ 3 ] in
           assert_equal ~printer:string_of_int_list_list
             [ [ 3 ]; [ 1; 3 ] ]
             result.sequence;
           assert_equal ~printer:string_of_int_array [| 0; 1; 0; 0 |]
             result.diagram;
           assert_equal ~printer:string_of_int_array [| 0; 1; 0; 2 |]
             result.joltage );
         ( "shortest sequence to target diagram" >:: fun _ ->
           let result = shortest_sequence_to_diagram machine in
           assert_equal ~printer:string_of_int 2 (result |> List.length) );
       ]

let _ = run_test_tt_main tests
