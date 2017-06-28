open Core;;

type tetramino = I | O | Z | S | J | L | T | None;;
type rotation = Left | Right | Mirrored | Normal;;

let advance_rotation rotation =
  match rotation with
  | Left -> Normal
  | Normal -> Right
  | Right -> Mirrored
  | Mirrored -> Left
;;

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
    mutable tetramino_rotation : rotation;
  }
;;

let cell_from_string string =
  match string with
  | "c" -> I
  | "y" -> O
  | "r" -> Z
  | "g" -> S
  | "b" -> J
  | "o" -> L
  | "m" -> T
  | "." | _ -> None
;;

let read_board board =
  (for i = 0 to (Array.length board)-1 do
     let line = (match In_channel.input_line(In_channel.stdin) with
       | Some x -> x
       | None -> "")
     in
     (* Split the line into a list of string tokens. Then map each
        token to its tetramino counterpart and assign this row to that
        map result. *)
     board.(i) <- Array.map (Array.of_list (String.split line ~on:' '))
                            ~f:(fun cell -> cell_from_string cell)
   done;
   board)
;;

let clear_board () = Array.make_matrix ~dimx:22 ~dimy:10 (None : tetramino)
;;

let print_cell tetramino =
  match tetramino with
  | I -> "c"
  | O -> "y"
  | Z -> "r"
  | S -> "g"
  | J -> "b"
  | L -> "o"
  | T -> "m"
  | None -> "."
;;

let print_board board =
  let num_cols = Array.length board.(0) in
  begin
  Array.iter board ~f:(fun row ->
    begin
      Array.iteri row ~f:(fun cindex cell ->
                   Out_channel.output_string Out_channel.stdout
                     (print_cell cell ^ if cindex = num_cols-1 then "" else " "));
      Out_channel.output_string Out_channel.stdout "\n";
      Out_channel.flush Out_channel.stdout;
    end);
  board
  end
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
       (if Array.for_all row ~f:(fun cell -> cell <> None) then
          (Array.fill row ~pos:0 ~len:(Array.length row) None;
           (game_data.score <- game_data.score + 100;
            game_data.lines_cleared <- game_data.lines_cleared + 1))
       ));
   (board, game_data))
;;

let print_tetramino tetramino_tuple =
  let tetramino_string =
    match tetramino_tuple with
    | (I, rotation) ->
       (match rotation with
        | Normal -> String.concat ~sep:"\n" [". . . .";
                                             "c c c c";
                                             ". . . .";
                                             ". . . ."]
        | Mirrored -> String.concat ~sep:"\n" [". . . .";
                                               ". . . .";
                                               "c c c c";
                                               ". . . ."]
        | Left -> String.concat ~sep:"\n" [". c . .";
                                           ". c . .";
                                           ". c . .";
                                           ". c . ."]
        | Right -> String.concat ~sep:"\n" [". . c .";
                                            ". . c .";
                                            ". . c .";
                                            ". . c ."])
    | (O, _) ->
       String.concat ~sep:"\n" ["y y";
                                "y y"]
    | (Z, rotation) ->
       (match rotation with
        | Normal -> String.concat ~sep:"\n" ["r r .";
                                             ". r r";
                                             ". . ."]
        | Mirrored -> String.concat ~sep:"\n" [". . .";
                                               "r r .";
                                               ". r r"]
        | Left -> String.concat ~sep:"\n" [". r .";
                                           "r r .";
                                           "r . ."]
        | Right -> String.concat ~sep:"\n" [". . r";
                                            ". r r";
                                            ". r ."])
    | (S, _) ->
       String.concat ~sep:"\n" [". g g";
                                "g g .";
                                ". . ."]
    | (J, _) ->
       String.concat ~sep:"\n" ["b . .";
                                "b b b";
                                ". . ."]
    | (L, _) ->
       String.concat ~sep:"\n" [". . o";
                                "o o o";
                                ". . ."]
    | (T, _) ->
       String.concat ~sep:"\n" [". m .";
                                "m m m";
                                ". . ."]
    | _ ->
       String.concat ~sep:"\n" [". . . .";
                                ". . . .";
                                ". . . .";
                                ". . . ."]
     in
     begin
       Out_channel.output_string Out_channel.stdout (tetramino_string ^ "\n");
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
  | Some x when x = ")" ->
     input_loop board
                {game_data with tetramino_rotation =
                                  (advance_rotation game_data.tetramino_rotation)}
                command_queue
  | Some x when x = "t" ->
     begin
       print_tetramino (game_data.active_tetramino, game_data.tetramino_rotation);
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
    tetramino_rotation = Normal;
  } in
input_loop (clear_board ()) game_data (Queue.create ());;
