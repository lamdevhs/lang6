let print_auto afn =
  let map, strlen = List.map, String.length in
  let is_in ys x = List.exists ((=) x) ys in
  let rec interval  a b =
    let rec fill result n = if a > n
      then result
      else fill (n :: result) (n-1) in
    fill [] b in
  let rec zip f xs ys = match (xs, ys) with
    | ([],_) -> []
    | (_,[]) -> []
    | (x::xs, y::ys) -> f x y :: zip f xs ys in
  let fuse f default_value = function
    | [] -> default_value 
    | x :: rest -> List.fold_left f x rest in
  let transits state letter =
      try state.tN letter with
      | Match_failure _ -> [] in
  let string_of_ints ints = match map string_of_int ints with
    | [] -> ""
    | [str] -> str
    | strlist ->
      "{" ^ fuse (fun a b -> a ^ "," ^ b) "" strlist ^ "}" in
  let table_of_automaton afn =
    let sigma = afn.sigmaN in
    (""::""::map(String.make 1)sigma) :: map (fun state_num -> (if (is_in afn.initN) state_num then "=>" else "") :: (let s = string_of_int state_num in
        if (afn.eN state_num).acceptN
        then "(" ^ s ^ ")" else s) :: (map ((fun state c -> string_of_ints (transits state c)) (afn.eN state_num)) sigma)) (interval 1 afn.nN) in
  let format_table table =
      let spaces n = String.make n ' ' in
        
      map (zip (fun size str -> let diff = (size + 2) - strlen str in
        let left = diff / 2 in
        (spaces left) ^ str ^ (spaces  (diff - left))) (fuse (zip max) [] (map (map strlen) table))) table in
    
    print_endline ("\n" ^ fuse (fun l1 l2 -> l1 ^ "|\n" ^ l2) "" (map (fuse (fun s1 s2 -> s1 ^ "|" ^ s2) "") (format_table (table_of_automaton afn))) ^ "|")
in
print_auto afnE4;;


