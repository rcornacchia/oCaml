(*
  Robert Cornacchia (rlc2160)
  PLT Hw1



  Problem 1 Source Code
 *)

let arg = [1; 1; 1; 3; 4; 1; 1];;

type 'a rle = One of 'a | Many of 'a * int;;

let rle list =
   (* categorize numbers into either one or many*)
   let categorize cnt elem =
     if cnt = 1 then One elem
     else Many (cnt, elem)
   in
   (* count (or tally) them based on frequency *)
   let rec tally count l = function
     (* Empty list returns an empty list *)
     | [] -> []
     (* an untyped element in the list gets typed*)
     | [x] -> (categorize (count+1) x) :: l
     (* a typed element in the list gets counted *)
     | hd :: (nextInt :: _ as tl) ->
         if hd = nextInt then tally (count + 1) l tl
         else tally 0 ((categorize (count + 1) hd) :: l) tl
in List.rev (tally 0 [] list);;

(* Call rle on arg *)
rle arg

(*
  Problem 1 Test - Run on Top Level


~/code/ocaml  $ ocaml
        OCaml version 4.02.1

# let arg = [1; 1; 1; 3; 4; 1; 1];;
val arg : int list = [1; 1; 1; 3; 4; 1; 1]
# type 'a rle = One of 'a | Many of 'a * int;;
type 'a rle = One of 'a | Many of 'a * int
# let rle list =
    (* categorize numbers into either one or many*)
    let categorize cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem)
    in
    (* count (or tally) them based on frequency *)
    let rec tally count acc = function
      | [] -> []
      | [x] -> (categorize (count+1) x) :: acc
      | hd :: (nextInt :: _ as tl) ->
          if hd = nextInt then tally (count + 1) acc tl
          else tally 0 ((categorize (count + 1) hd) :: acc) tl
in List.rev (tally 0 [] list);;
val rle : int list -> int rle list = <fun>
# rle arg;;
- : int rle list = [Many (3, 1); One 3; One 4; Many (2, 1)]
#

///////////////////////////////////////////////////////
///////////////////////////////////////////////////////

Problem 2 Source

*)


{
  module StringMap = Map.Make (String)
  type token = EOF | Word of string
}


rule token = parse
   | eof { EOF }
   | ['a'-'z' 'A'-'Z']+ as word { Word(word) }
   | _ { token lexbuf }

{
  (* Get words from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  let wordlist =
    let rec next l = match token lexbuf with
                     EOF -> l
                     | Word(s) -> next (s :: l)
     in next []
  in

  (* Create String map out of list with the count for each word *)
  let map =
    let rec count wordlist map =
      match wordlist with
      | [] -> map     (* wordlist is empty, so just return map *)
      (* check to see if the headlist is already in the map *)
      | hd::tl -> count tl (StringMap.add hd
                 (* if it is, just increment the counter already there *)
                 (if StringMap.mem hd map then (StringMap.find hd map) + 1
                 (* otherwise set counter to 1 *)
                  else 1)
             map)
    in count wordlist StringMap.empty
  in

  (* This folding function takes the map and converts to a list of word, count pairs *)
  let foldingFunction =
    (fun word count x -> (count, word) :: x)
  in

  (* Use StringMap.fold to convert the map to
    list of (count, word) pairs*)
  let wordcounts =
    StringMap.fold foldingFunction map []
  in

  (* Sort the pairs using List.sort
     This code was given *)
  let wordcounts =
    List.sort (fun (c1, _) (c2, _) ->
               Pervasives.compare c2 c1)
  wordcounts in

  (* Print the list with List.iter
    the list contains tuples ()*)
  let print list =
    List.iter (fun item -> print_endline(string_of_int (fst item) ^ " " ^ snd item)) list
  in
  print wordcounts;;

}

(* Makefile used in Problem 2

ocamllex wordcount.mll
echo " "
ocamlc -o wordcount wordcount.ml
echo " "
./wordcount < wordcount.mll
echo " " *)




(* Problem 2 Test


~/code/ocaml  $ ./make.sh
4 states, 315 transitions, table size 1284 bytes


9 map
9 let
9 in
7 count
7 StringMap
6 word
6 list
5 the
4 wordlist
4 wordcounts
4 with
4 token
4 of
4 hd
4 c
4 List
3 print
3 next
3 lexbuf
3 l
3 item
3 fun
3 Word
3 EOF
2 x
2 to
2 tl
2 string
2 stdin
2 sort
2 s
2 rec
2 pairs
2 match
2 iter
2 from
2 foldingFunction

///////////////////////////////////////////////////////
///////////////////////////////////////////////////////


Problem 3 Source Code

calc.ml *)

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

(* Parser.mly  *)

%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EQUALS COMMA EOF
%token <int> LITERAL VARIABLE

%left COMMA
%right EQUALS
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr     { Binop($1, Add, $3) }
| expr MINUS  expr     { Binop($1, Sub, $3) }
| expr TIMES  expr     { Binop($1, Mul, $3) }
| expr DIVIDE expr     { Binop($1, Div, $3) }
| LITERAL              { Lit($1) }

/*(* The following cases were not in the lecture slides
    handles commas, variable assignment, and
    calls for the variable *)*/
| expr COMMA expr      { Seq($1, $3) }
| VARIABLE             { Var($1) }
| VARIABLE EQUALS expr { Asn($1, $3) }


(* Scanner.mll *)
{ open Parser }

rule token =
  parse [' ' '\t' '\r' '\n']  { token lexbuf }
      | '+'                   { PLUS }
      | '-'                   { MINUS }
      | '*'                   { TIMES }
      | '/'                   { DIVIDE }
      | ['0'-'9']+ as lit     { LITERAL(int_of_string lit) }
      | ['a'-'z'] as lit      { VARIABLE(int_of_char lit - 97) }
      | '='                   { EQUALS}
      | ','                   { COMMA }
      | eof                   { EOF }

(* ast.mli *)
type operator = Add | Sub | Mul | Div
   type expr =
       Binop of expr * operator * expr
     | Lit of int
     | Seq of expr * expr
     | Asn of int * expr
     | Var of int
(*
  Part 3 Test

  Compiled according to instructions on slides.
  Used Cmd+D to stop input

  ~/code/ocaml  $ ./calc
  a = b = 3, b = b + 3, a * b + 2
  20

  ~/code/ocaml  $ ./calc
  a = 2, a * 4
  8D
  The D was printed, but the correct answer, 8 was also printed.
  The D must be from Cmd+D
*)
