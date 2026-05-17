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
let input_line_example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"

let tests =
  "factory"
  >::: [
         ( "parsing input line" >:: fun _ ->
           let machine = parse_input input_line_example in
           assert_equal ~printer:string_of_int_array [| 3; 5; 4; 7 |]
             machine.joltage;
           assert_equal ~printer:string_of_int_list_list
             [ [ 3 ]; [ 1; 3 ]; [ 2 ]; [ 2; 3 ]; [ 0; 2 ]; [ 0; 1 ] ]
             machine.buttons );
       ]

let _ = run_test_tt_main tests
