open Printf

type machine = { diagram : int; buttons : int list list; joltage : int list }

let parse_input input_line =
  let parse_diagram s =
    let chars = s |> String.to_seq |> List.of_seq in
    fst
      (List.fold_left
         (fun (value, power2) ch ->
           match ch with
           | '#' -> (value lor power2, power2 * 2)
           | '.' -> (value, power2 * 2)
           | _ -> (value, power2))
         (0, 1) chars)
  in
  let parse_int_list s =
    String.sub s 1 (String.length s - 2)
    |> String.split_on_char ',' |> List.map int_of_string
  in
  let parse_machine s =
    let words = String.split_on_char ' ' s in
    let diagram = parse_diagram (List.hd words) in
    let buttons =
      List.map
        (fun s -> parse_int_list s)
        (List.drop 1 (List.take (List.length words - 1) words))
    in
    let joltage = parse_int_list (List.hd (List.rev words)) in
    { diagram; buttons; joltage }
  in
  parse_machine input_line

let matrix_of_machine machine =
  let rows = List.length machine.joltage in
  let cols = List.length machine.buttons in
  let matrix = Array.make_matrix rows (cols + 1) 0 in
  machine.buttons
  |> List.iteri (fun col l ->
      l |> List.iter (fun row -> matrix.(row).(col) <- 1));
  machine.joltage |> List.iteri (fun row v -> matrix.(row).(cols) <- v);
  matrix

let matrix_reduce matrix =
  let rows = matrix |> Array.length in
  let cols = matrix.(0) |> Array.length in

  let rec find_pivot row col =
    if row < rows then
      if matrix.(row).(col) = 0 then find_pivot (row + 1) col else Some row
    else None
  in

  let swap_rows a b =
    if a <> b then
      for col = 0 to cols - 1 do
        let tmp = matrix.(b).(col) in
        matrix.(b).(col) <- matrix.(a).(col);
        matrix.(a).(col) <- tmp
      done
  in

  let reduce_row p i = swap_rows p i in

  for i = 0 to (matrix |> Array.length) - 1 do
    match find_pivot i i with Some p -> reduce_row p i | None -> ()
  done;
  matrix
