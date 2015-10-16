open Ast

(*
  accept empty array
  binop taken from lecture slides
  letter vars are stored in array
*)
let rec eval arr = function
    Lit(x) -> x, arr
  | Binop(e1, op, e2) ->
      let v1, arr = eval arr e1 in
      let v2, arr = eval arr e2 in
      (match op with
        Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2), arr
  | Var(i) ->
      arr.(i), arr
  | Seq(e1, e2) ->
      let _, arr = eval arr e1 in
      eval arr e2
  | Asn(i, e) ->
      let v, arr = eval arr e in
      let _ = arr.(i) <- v in
      v, arr

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  (*  Create an arr to hold letter vars
      One index for each letter (only lowercase)
  *)
  let arr = Array.make 26 0 in
  let result = eval arr expr in
  print_endline (string_of_int (fst result))


(* ------------- TESTS -------------

To test this code, I compiled the calculator and ran the following,
using Ctrl-D to end the input:

$ ./calc
$2 = $1 = 3, $3 = 6, $1 * $2 + $3 + 4
19

$ ./calc
1 + 1 * 5 * 4 / 2
11

$ ./calc
$6 = 3, $7 = 9, 5 * $7 / $6
15

$ ./calc
0 - 4 - 1 + 3
-2

----------- END TESTS ----------- *)
