let seen = Hashtbl.create 0
let () =
  Stream.iter
    (fun word -&gt;
       Hashtbl.replace seen word
         (try Hashtbl.find seen word + 1
          with Not_found -&gt; 1))
    (word_stream_of_channel stdin)

(* output hash in a descending numeric sort of its values *)
let () =
  let words = ref [] in
  Hashtbl.iter (fun word _ -&gt; words := word :: !words) seen;
  List.iter
    (fun word -&gt;
       Printf.printf "%5d %s\n" (Hashtbl.find seen word) word)
    (List.sort
       (fun a b -&gt; compare (Hashtbl.find seen b) (Hashtbl.find seen a))
       !words)
