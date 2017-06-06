open Core

type game_info =
  {
    mutable score : int;
    mutable lines_cleared : int;
  }
;;

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

let clear_board () = [|
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
  |]
;;

let print_board board =
  let num_rows = 22 in
  let num_cols = 10 in
  (for i = 0 to num_rows - 1 do
     for j = 0 to num_cols - 1 do
       Out_channel.output_string Out_channel.stdout (board.(i).(j) ^ (if j=num_cols-1 then ""
                                                                      else " "))
     done;
     Out_channel.output_string Out_channel.stdout "\n";
     Out_channel.flush Out_channel.stdout;
   done;
   board)
;;

let process_query query_string board game_data =
  begin
    (match String.drop_prefix query_string 1 with
    | "s" -> begin
        Out_channel.output_string Out_channel.stdout (string_of_int game_data.score)
      end
    | "n" -> begin
        Out_channel.output_string Out_channel.stdout (string_of_int game_data.lines_cleared)
      end
    | _ -> ()
    );
    Out_channel.output_string Out_channel.stdout "\n";
    Out_channel.flush Out_channel.stdout;
    board
  end
;;

let one_step board game_data =
  (Array.iter board ~f:(fun row ->
       (if Array.for_all row ~f:(fun cell -> cell <> ".") then
          (Array.fill row ~pos:0 ~len:(Array.length row) ".";
           (game_data.score <- game_data.score + 100;
            game_data.lines_cleared <- game_data.lines_cleared + 1))
       ));
   (board, game_data))
;;

let rec input_loop board game_data =
  match In_channel.input_line In_channel.stdin with
  | Some x when x = "q" -> None
  | Some x when x = "p" ->
     input_loop (print_board board) game_data
  | Some x when x = "g" ->
     input_loop (read_board board) game_data
  | Some x when x = "c" ->
     input_loop (clear_board ()) game_data
  | Some x when x = "s" ->
     let (new_board, new_data) = one_step board game_data in
     input_loop new_board new_data
  | Some x when (String.is_prefix x ~prefix:"?") ->
     input_loop (process_query x board game_data) game_data
  | None | Some _ -> input_loop board game_data

;;

let game_data = {
    score = 0;
    lines_cleared = 0;
  } in
input_loop (clear_board ()) game_data ;;
