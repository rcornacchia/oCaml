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
      | [] -> []
      | [x] -> (categorize (count+1) x) :: l
      | hd :: (nextInt :: _ as tl) ->
          if hd = nextInt then tally (count + 1) l tl
          else tally 0 ((categorize (count + 1) hd) :: l) tl
in List.rev (tally 0 [] list);;

rle arg


(* val encode : 'a list -> 'a rle list = <fun> *)

let x = rle arg;;
