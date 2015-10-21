(*
    Alden Quimby - adq2101 - CS4115 HW1
    3: calc.ml
*)

open Ast

(*
    Literals and Binary Operators were taken from example.
    For sequences, evaluate expressions in order.
    For assignment, set values in the env array.
    For variable access, read from the env array.
*)
let rec eval env = function
    Lit(x) -> x, env
  | Binop(e1, op, e2) ->
      let v1, env = eval env e1 in
      let v2, env = eval env e2 in
      (match op with
        Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2), env
  | Seq(e1, e2) ->
      let _, env = eval env e1 in
      eval env e2
  | Asn(idx, e) ->
      let v, env = eval env e in
      let _ = env.(idx) <- v in
      v, env
  | Var(idx) ->
      env.(idx), env

(*
    Read from stdin, call the scanner and parser to create an expression,
    create the "environment" (array of 10 zeros to hold the variables),
    evaluate the expression, and print the result.
*)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let env = Array.make 10 0 in
  let result = eval env expr in
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
