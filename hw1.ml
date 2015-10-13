
let arg = [1; 1; 1; 3; 4; 1; 1];;
type 'a rle =  One of 'a | Many of 'a * int;;

(*
    001   if n = n+1                    increment counter
    002   else if counter = 1           create one object : "One(n)", add to list
    003   else                          create Many object: "Many(n, counter)", add to list
*)

let length = List.length arg;;
print_int length;;

for i = 0 to length - 2 do
    (* let y = List.nth arg i;
    let j = i+1; *)
    (* let z = List.nth arg 2 *)
    print_int 3;
done;;
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
