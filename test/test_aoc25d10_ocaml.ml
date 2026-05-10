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
      "[|";
      String.concat ";" (Array.to_list (Array.map string_of_int a));
      "|]\n";
    ]

let string_of_int_array_array a =
  String.concat ""
    [
      "[|";
      String.concat ";" (List.map string_of_int_array (Array.to_list a));
      "|]";
    ]

let matrix_of_list l =
  let rows = l |> List.length in
  let cols = l |> List.hd |> List.length in
  let matrix = Array.make_matrix rows cols 0 in
  l
  |> List.iteri (fun row values ->
      values |> List.iteri (fun col value -> matrix.(row).(col) <- value));
  matrix

let parse_input = Aoc25d10_ocaml.Factory.parse_input
let matrix_of_machine = Aoc25d10_ocaml.Factory.matrix_of_machine
let matrix_reduce = Aoc25d10_ocaml.Factory.matrix_reduce
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
         ( "reducing a very simple matrix" >:: fun _ ->
           let initial =
             matrix_of_list [ [ 0; 0; 1; 2 ]; [ 0; 1; 1; 3 ]; [ 1; 0; 0; 4 ] ]
           in
           let expected =
             matrix_of_list [ [ 1; 0; 0; 4 ]; [ 0; 1; 1; 3 ]; [ 0; 0; 1; 2 ] ]
           in
           let result = matrix_reduce initial in
           assert_equal ~printer:string_of_int_array_array expected result );
         ( "reducing a matrix with subtractions" >:: fun _ ->
           let initial =
             matrix_of_list
               [
                 [ 0; 0; 1; 0; 2 ];
                 [ 0; 1; 1; 0; 3 ];
                 [ 1; 0; 0; 1; 4 ];
                 [ 1; 0; 1; 0; 5 ];
               ]
           in
           let expected =
             matrix_of_list
               [
                 [ 1; 0; 0; 1; 4 ];
                 [ 0; 1; 1; 0; 3 ];
                 [ 0; 0; 1; 0; 2 ];
                 [ 0; 0; 0; 1; 1 ];
               ]
           in
           let result = matrix_reduce initial in
           assert_equal ~printer:string_of_int_array_array expected result );
         ( "reducing a vertically extended matrix" >:: fun _ ->
           let initial =
             matrix_of_list
               [
                 [ 1; 1; 0; 0; 12 ];
                 [ 0; 1; 0; 1; 5 ];
                 [ 0; 0; 1; 0; 2 ];
                 [ 0; 0; 1; 0; 2 ];
               ]
           in
           let expected =
             matrix_of_list
               [
                 [ 1; 1; 0; 0; 12 ];
                 [ 0; 1; 0; 1; 5 ];
                 [ 0; 0; 1; 0; 2 ];
                 [ 0; 0; 0; 0; 0 ];
               ]
           in
           let result = matrix_reduce initial in
           assert_equal ~printer:string_of_int_array_array expected result );
         ( "reducing a horizontally extended matrix" >:: fun _ ->
           let initial =
             matrix_of_list
               [
                 [ 1; 1; 0; 1; 1; 0; 54 ];
                 [ 0; 1; 0; 1; 0; 1; 21 ];
                 [ 1; 0; 0; 0; 1; 0; 34 ];
                 [ 1; 1; 0; 1; 0; 1; 53 ];
               ]
           in
           let expected =
             matrix_of_list
               [
                 [ 1; 1; 0; 1; 1; 0; 54 ];
                 [ 0; 1; 0; 1; 0; 1; 21 ];
                 [ 0; 0; 0; 0; 0; 1; 1 ];
                 [ 0; 0; 0; 0; 1; -1; 1 ];
               ]
           in
           let result = matrix_reduce initial in
           assert_equal ~printer:string_of_int_array_array expected result );
       ]

let _ = run_test_tt_main tests
