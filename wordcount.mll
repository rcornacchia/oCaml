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
