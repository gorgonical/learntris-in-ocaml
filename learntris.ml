open Core;;

type tetramino = I | O | Z | S | J | L | T | None;;

let is_tetramino x =
  match x with
  | "I" | "O" | "Z" | "S" | "J" | "L" | "T" -> true
  | _ -> false
;;

let tetramino_from_string string =
  match string with
  | "I" -> I
  | "O" -> O
  | "Z" -> Z
  | "S" -> S
  | "J" -> J
  | "L" -> L
  | "T" -> T
  | _ -> None
;;

type game_info =
  {
    mutable score : int;
    mutable lines_cleared : int;
    mutable active_tetramino : tetramino;
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

let print_tetramino tetramino =
  let tetramino_string =
    (match tetramino with
    | I ->
        (". . . ." ^ "\n" ^
         "c c c c" ^ "\n" ^
         ". . . ." ^ "\n" ^
         ". . . ." ^ "\n")
    | O ->
        ("y y" ^ "\n" ^
         "y y")
    | Z ->
        ("r r ." ^ "\n" ^
         ". r r" ^ "\n" ^
         ". . ." ^ "\n")
    | S ->
       (". g g" ^ "\n" ^
        "g g ." ^ "\n" ^
        ". . ." ^ "\n")
    | J ->
       ("b . ." ^ "\n" ^
        "b b b" ^ "\n" ^
        ". . ." ^ "\n")
    | L ->
       (". . o" ^ "\n" ^
        "o o o" ^ "\n" ^
        ". . ." ^ "\n")
    | T ->
       (". m ." ^ "\n" ^
        "m m m" ^ "\n" ^
        ". . ." ^ "\n")
    | _ ->
        (". . . ." ^ "\n" ^
         ". . . ." ^ "\n" ^
         ". . . ." ^ "\n" ^
         ". . . ." ^ "\n")) in
  begin
    Out_channel.output_string Out_channel.stdout tetramino_string;
    Out_channel.flush Out_channel.stdout
  end
;;

let rec condense_list list =
  match list with
  | [] -> []
  | sole::[] -> sole::[]
  | first::second::[] -> (if first = second then first::[]
                          else first::second::[])
  | first::second::rest -> (if first = second then (condense_list (first::rest))
                                else first::(condense_list (second::rest)))
;;

let get_input command_queue =
  if Queue.length command_queue = 0 then
    begin
      match In_channel.input_line In_channel.stdin with
      | Some x ->
         let command_list = String.split ~on:' ' x in
         let condensed_list = condense_list command_list in
         begin
           List.iter condensed_list ~f:(fun elem -> Queue.enqueue command_queue elem);
           Queue.dequeue command_queue
         end
      | None -> None
    end
  else Queue.dequeue command_queue
;;

let rec input_loop board game_data command_queue =
  match get_input command_queue with
  | Some x when x = "q" -> None
  | Some x when x = "p" ->
     input_loop (print_board board) game_data command_queue
  | Some x when x = "g" ->
     input_loop (read_board board) game_data command_queue
  | Some x when x = "c" ->
     input_loop (clear_board ()) game_data command_queue
  | Some x when x = "s" ->
     let (new_board, new_data) = one_step board game_data in
     input_loop new_board new_data command_queue
  | Some x when x = "t" ->
     begin
       print_tetramino game_data.active_tetramino;
       input_loop board game_data command_queue
     end
  | Some x when is_tetramino x ->
     input_loop board {game_data with active_tetramino = tetramino_from_string x;} command_queue
  | Some x when (String.is_prefix x ~prefix:"?") ->
     input_loop (process_query x board game_data) game_data command_queue
  | None -> input_loop board game_data command_queue
  | Some _ -> None
;;

let game_data = {
    score = 0;
    lines_cleared = 0;
    active_tetramino = None;
  } in
input_loop (clear_board ()) game_data (Queue.create ());;
