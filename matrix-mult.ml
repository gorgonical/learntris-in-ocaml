open Core

type multiplication_result =
  | None
  | Some of int array array
;;

let make_matrix m n =
  Array.make_matrix ~dimx:m ~dimy:n 0
;;

let transpose matrix =
  let transposed_matrix = Array.create ~len:(Array.length matrix) [|0|] in
  begin
    (* Get the length of any of the sub-arrays *)
    for i = 0 to (Array.length matrix.(0)) - 1 do
      Array.set transposed_matrix i (Array.map matrix ~f:(fun row -> Array.get row i))
    done
    ; transposed_matrix
  end
;;

let get_column matrix n =
  let height = Array.length matrix in
  let column = Array.create ~len:height 0 in
  begin
    for i = 0 to (Array.length matrix)-1 do
      Array.set column i matrix.(i).(n)
    done;
    column
  end
;;

let dot_product x y =
  Array.map2_exn x y ~f:(fun x y -> x * y)
;;

let get_row matrix n =
  Array.get matrix n
;;

let get_multiplied_row row columns =
  Array.map columns ~f:(fun column ->
              Array.fold ~init:0 (Array.map2_exn row column ~f:( * )) ~f:(+))
;;

let multiply left right =
  if Array.length left.(0) <> Array.length right then None
  else
    let matrix = make_matrix (Array.length left) (Array.length right.(0)) in
    let transposed_right = transpose right in
    begin
      Array.iteri left ~f:(fun i row ->
                    Array.set matrix i (get_multiplied_row row transposed_right))
    ; Some matrix
    end
;;

let print_array array =
  Out_channel.output_string Out_channel.stdout ((Array.fold array ~init:""
                                                            ~f:(fun acc a -> acc ^ " " ^ (string_of_int a))) ^ "\n")
;;

let print_matrix matrix =
  Array.iter matrix ~f:print_array
;;

let left = [|[|1;2|];
             [|3;4|]|] in
let right = [|[|5;6|];
              [|7;8|]|] in

let matrix =
match (multiply left right ) with
| None -> [|[|0|]|]
| Some x -> x in
print_matrix matrix;
