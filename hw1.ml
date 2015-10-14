let arg = [1; 1; 1; 3; 4; 1; 1];;

type 'a rle = One of 'a | Many of 'a * int

let encode l =
    let create_tuple cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem) in
    let rec aux count acc = function
      | [] -> []
      | [x] -> (create_tuple (count+1) x) :: acc
      | hd :: (snd :: _ as tl) ->
          if hd = snd then aux (count + 1) acc tl
          else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
        List.rev (aux 0 [] l);;



(* val encode : 'a list -> 'a rle list = <fun> *)

let x = encode arg;;

let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

print_list x;;

(* let arg = [1; 1; 1; 3; 4; 1; 1];;
type 'a rle =  One of 'a | Many of 'a * int;;

let rle list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count+1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count+1,a) :: acc) t in
    List.rev (aux 0 [] list);;
rle arg;; *)

(*
    001   if n = n+1                    increment counter
    002   else if counter = 1           create one object : "One(n)", add to list
    003   else                          create Many object: "Many(n, counter)", add to list
*)

(* let length = List.length arg;;
print_int length;;

(* for i = 0 to length - 2 do
    (* let y = List.nth arg i;
    let j = i+1; *)
    (* let z = List.nth arg 2 *)
    print_int 3;
done;; *)
(*
let checkCounter y z =
print_int y;
print_int z;;
(* if !counter = 0 then counter := !counter + 1
else if !counter >= 1;; *)

let rle arg = *)



(*  check if last element is equal to second to last element, if so then *)

List.iter (fun n, a -> checkCounter n, a; ) arg;;

rle arg;;
 *)

(*
let counter = ref 0;;

print_int !counter;;


let rle arg =
List.iter (fun n -> checkCounter n; print_newline()) arg ;;


print_int !counter;; *)


(*
let counter = ref 0;;
(* print_int counter;; *)

if !counter = 0 then !counter := !counter + 1;;
print_int !counter;; *)



(*
(* List.iter (fun n -> ) *)
let countChecker = ;;



 *)



(*
let rle [1; 1; 1; 3; 4; 1; 1] =
      [Many(1, 3); One(3); One(4); Many(1, 2)] *)
