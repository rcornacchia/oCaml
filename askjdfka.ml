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

  (* Initialize emty string map *)
  let map = StringMap.empty

  (* Create String map out of list with the count for each word *)
  let map =
    let rec count wordlist map = match wordlist with
      | [] -> map
      | hd::tl -> count tl (StringMap.add hd (if StringMap.mem hd map then (StringMap.find hd map) + 1 else 1) map)
    in count wordlist StringMap.empty
  in






  (* Use StringMap.fold to convert the map to
    list of (count, word) pairs*)

  let wordcounts =
    StringMap.fold (fun word num x -> (num, word) :: x) map []
  in

  (* Sort the pairs using List.sort
     This code was given *)
  let wordcounts =
    List.sort (fun (c1, _) (c2, _) ->
               Pervasives.compare c2 c1)
  wordcounts in

  (* Print the list with List.iter
    the list contains tuples ()*)

  List.iter (fun word -> print_endline(string_of_int (fst word) ^ " " ^ snd word)) wordcounts;;

}
