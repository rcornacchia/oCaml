module StringMap = Map.Make(String);;

let wordlist = [Some "Hello"; Some "World"];;

let rec count map wordlist =
  match wordlist with
  | [] -> map
  | hd :: tl -> wordlist

let map = count StringMap.empty wordlist;;

let count_words (f: unit -> string option) =
let rec loop cnt =
  match (f ()) with
  | None ->
  List.rev
  (StringMap.fold (fun k c l -> (k, c) :: l) cnt [])
  | Some(word) ->
  let cnt =
    try
      let c = StringMap.find word cnt in
      StringMap.add word (c+1) cnt
      with
      | Not_found -> StringMap.add word 1 cnt
      in
loop cnt
in
loop StringMap.empty;;

count_words worl;;
