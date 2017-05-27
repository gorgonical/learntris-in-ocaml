open Core

let read_board board =
  (for i = 0 to 21 do
     let line = (match In_channel.input_line(In_channel.stdin) with
       | Some x -> x
       | None -> "")
     in
     board.(i) <- Array.of_list (String.split line ~on:' ')
   done;
   board)
;;

let rec input_loop board =
  let num_rows = 22 in
  let num_cols = 10 in
  match In_channel.input_line In_channel.stdin with
  | Some x when x = "q" -> None
  | Some x when x = "p" ->
     (for i = 0 to num_rows - 1 do
        for j = 0 to num_cols - 1 do
          (* Pad only the spaces in the middle *)
          Out_channel.output_string Out_channel.stdout (board.(i).(j) ^ (if j=num_cols-1 then ""
                                                                         else " "))
        done;
        Out_channel.output_string Out_channel.stdout "\n";
        Out_channel.flush Out_channel.stdout;
      done;
      input_loop board)
  | Some x when x = "g" ->
     input_loop (read_board board)
  | None | Some _ -> input_loop board
;;

let board = [|
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
      [|".";".";".";".";".";".";".";".";".";"."|];
  |] in
    input_loop board ;;
