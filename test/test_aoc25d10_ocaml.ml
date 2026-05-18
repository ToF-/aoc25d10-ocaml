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

let second_line_example =
  "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"

let machine = parse_input input_line_example
let second_machine = parse_input second_line_example
let sample = "../testdata/sample.txt"
let input = "../testdata/input.txt"

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
         ( "shortest sequence to machine diagram" >:: fun _ ->
           let result = shortest_sequence_to_diagram machine in
           assert_equal ~printer:string_of_int 2 (result |> List.length) );
         ( "sum of shortests sequence lengths on sample" >:: fun _ ->
           let result = solution_a sample in
           assert_equal ~printer:string_of_int 7 result );
         (*( "sum of shortests sequence lengths on input" >:: fun _ ->
           let result = solution_a input in
           assert_equal ~printer:string_of_int 512 result );*)
         ( "shortest sequence to a target diagram" >:: fun _ ->
           let result =
             shortest_sequence_to_diagram ~diagram:[| 0; 0; 0; 1 |] machine
           in
           assert_equal ~printer:string_of_int 1 (result |> List.length) );
         ( "shortest sequence to joltage" >:: fun _ ->
           let result = shortest_sequence_length_to_joltage machine in
           assert_equal ~printer:string_of_int 10 result );
         ( "shortest sequence to joltage 2nd example" >:: fun _ ->
           let result = shortest_sequence_length_to_joltage second_machine in
           assert_equal ~printer:string_of_int 12 result );
         ( "sum of shortests sequence to joltage on sample" >:: fun _ ->
           let result = solution_b sample in
           assert_equal ~printer:string_of_int 34 result );
       ]

let _ = run_test_tt_main tests
