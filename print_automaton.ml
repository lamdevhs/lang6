
let map = List.map;;

(* ******************************************************************* *)
 (* Bibliothèque sur les listes *)  
let rec appartient = function 
(a,b::l)-> if a=b then true else appartient(a,l)
|(_,[])-> false;;
appartient : 'a * 'a list -> bool = <fun>

let rec union l = function 
(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
| []->l;;
union : 'a list -> 'a list -> 'a list = <fun>


(* sur les chaines *)
let string_of_char = String.make 1 ;;


(* ******************************************************************* *)
(*    Un type pour les AFN  *)

type etatN = {acceptN : bool ; tN : char -> int list};;

type afn = {sigmaN: char list; (* l'alphabet *)
      nN: int; (* Q est l'ensemble {1..N} *)
      initN: int list; (* les états initiaux *)
      eN : int -> etatN};; 
      
      
(*    Lecture d'un mot par un AFN     *)

let an1  = {sigmaN= ['a';'b'] ; nN = 3; initN = [1] ; 
      eN = function 
          1 -> {acceptN = false ;
              tN = function 
                 'a'->[2] }
        |2 -> {acceptN = true ;
              tN = function 
                 'a'->[2] 
               |'b'-> [2;3] }      
        |3 -> {acceptN = false ;
              tN = function 
                 'a'->[2]
               |'b'->[3]   }             
    };;


let transition : etatN -> char -> int list =
  fun state letter ->
    try state.tN letter with
    | Match_failure _ -> []
;;

let rec unique_foldmap : ('a -> 'b list) -> 'a list -> 'b list =
  fun f xs -> match xs with
    | [] -> []
    | x::tail -> union (f x) (unique_foldmap f tail)
;;

let rec readN_onestate : afn -> char -> int -> int list =
  fun afn l state_num ->
    let state = afn.eN state_num in
    transition state l
    ;;

let rec readN : afn -> string -> int list -> int list =
  fun afn word state_num_list ->
    if state_num_list = [] then [] else
    match word with
    | "" -> state_num_list
    | w ->
      let l = w.[0]
      and tail = String.sub w 1 (String.length w - 1) in
      let new_state_num_list =
        unique_foldmap
          (readN_onestate afn l)
          state_num_list in
      readN afn tail new_state_num_list
    ;;

readN an1 "abba" [1];;

let rec contains ls x = match ls with
    | [] -> false
    | y :: ys -> (y = x) || contains ys x
;;

let acceptN : afn -> string -> bool =
  fun afn word ->
    let final_states = readN afn word afn.initN in
    let rec any_of = (function
      | [] -> false
      | false :: tail -> any_of tail
      | true :: _ -> true)
    and does_accept state_num = (afn.eN state_num).acceptN in
    any_of (List.map does_accept final_states)
;;

acceptN an1 "abba";;



(* automate du td4 exo 3 *)
let an2  = {sigmaN= ['a';'b';'c'] ; nN = 5; initN = [1] ; 
      eN = function 
          1 -> {acceptN = false ;
              tN = function 
                 'a'-> [1;2] | 'c' -> [4] }
        |2 -> {acceptN = false ;
              tN = function 
                 'b'-> [3;5] }      
        |3 -> {acceptN = false ;
              tN = function 
                'a'->[4]
               |'c'->[1;5]   }             
        |4 -> {acceptN = true ;
              tN = function
                |'c' -> [4] }
        |5 -> {acceptN = true ;
              tN = function
                |'c' -> [2] }
    };;

let first_nchars str n = String.sub str 0 n ;;

(* retourne la liste des lectures partielles : c'est une liste de listes,
chaque liste contenant l'ensemble des etats accessibles apres lecture
des n premieres lettres, pour n entre 0 et (longueur du mot - 1).
cela correspond donc au graphe de toutes les lectures,
"écrasé" verticalement. c'est aussi la liste de lecture de l'automate
des parties. *)
let all_readings afn word =
  let rec go k acc =
    if k = 0 then afn.initN :: acc else
    let part = first_nchars word k in
    let partial_reading = readN afn part afn.initN in
    go (k-1) (partial_reading :: acc)
  in go (String.length word) []
;;

let words = ["abc"; "baabc"; "aabcbcab"; "abcabcca"];;
let readings = List.map (all_readings an2) words ;;
(* val readings : int list list list =
  [[[1]; [1; 2]; [3; 5]; [2; 1; 5]]; <- arbre de lecture écrasé de abc
  
  [[1]; []; []; []; []; []]; <--- arbre de lecture écrasé de baabc
  
  [[1]; [1; 2]; [1; 2]; [3; 5]; [2; 1; 5];
  [3; 5]; [2; 1; 5]; [1; 2]; [3; 5]]; <--- arbre de lecture écrasé de baabc
  
   [[1]; [1; 2]; [3; 5]; [2; 1; 5]; [1; 2];
   [3; 5]; [2; 1; 5]; [2; 4]; []]] <--- arbre de lecture écrasé de baabc
 *)
let results = List.map (acceptN an2) words ;;

let explore =
  fun afn letter state_num ->
  transition (afn.eN state_num) letter
;;
  

let rec cloture eps afn cl =
    let next_cl = unique_foldmap (explore afn eps) cl in
    let new_cl = union cl next_cl in
    if List.length new_cl = List.length cl
      then new_cl (* la cloture est stable par epsilon transition *)
      else cloture eps afn new_cl
        (* ^ on n'a pas encore toute la cloture *)
  ;;

let rec any_of = function
      | [] -> false
      | false :: tail -> any_of tail
      | true :: _ -> true
;;

let heritage_acceptance afn clot =
  let bool_list =
    List.map
      (fun state_num -> (afn.eN state_num).acceptN)
      clot
  in
  any_of bool_list
;;
  

let elim_eps_one_state eps afn state_num =
  let clot = cloture eps afn [state_num] in
  let rec make_new_tN sigma tN = 
    match sigma with
    | [] -> tN
    | this_letter :: tail ->
        if this_letter = eps then make_new_tN tail tN else
        let new_transitions = unique_foldmap (explore afn this_letter) clot in
        let new_tN = fun ch ->
          if ch = this_letter
          then new_transitions
          else (tN ch)
        in
        make_new_tN tail new_tN
  in
  let empty_tN = fun _ -> [] in
  let new_tN = make_new_tN afn.sigmaN empty_tN in
  {acceptN = (heritage_acceptance afn clot) ; tN = new_tN}
;;
        
  

let elim_eps_eN eps afn =
  let nb_states = afn.nN
  and first_new_eN =
    let new_state_1 = elim_eps_one_state eps afn 1 in
    fun 1 -> new_state_1
  in
  let rec make_new_eN this_state_num eN =
    if this_state_num > nb_states then eN else
    let this_new_state =
      elim_eps_one_state eps afn this_state_num
    in
    let new_eN = fun state_num ->
      if state_num = this_state_num
      then this_new_state
      else (eN state_num)
    in
    make_new_eN (this_state_num + 1) new_eN
  in
  make_new_eN 2 first_new_eN
;;
  
  

let elim_eps : char -> afn -> afn =
  fun eps afn ->
    let new_sigmaN = List.filter (fun c -> c <> eps) afn.sigmaN
    and new_nN = afn.nN
    and new_eN = elim_eps_eN eps afn in
    { sigmaN = new_sigmaN; nN = new_nN;
      initN = afn.initN; eN = new_eN }
;;

(* Fonction `reduce` : (aussi appele `fold_left`)
prend une fonction a deux parametres, une liste, une valeur
d'accumulation ('acc'), et retourne une valeur calculee
en combinant recursivement chaque element de la liste
avec la valeur d'accumulation.
exemples:
somme des elements d'une liste :
  reduce (fun acc y -> acc + y) [a,b,c,d] 0
  == ((((0 + a) + b) + c) + d)
renverser l'ordre des elements d'une liste :
  reduce (fun acc y -> y :: acc) [a,b,c,d] []
  == d :: (c :: (b :: (a :: [])))
OU de tous les elements d'une liste :
  reduce (fun acc y -> y || acc) [true;false;true;true] false
  == d || (c || (b || (a || false))) *)
let rec reduce : ('res -> 'x -> 'res) -> 'res -> 'x list -> 'res =
  fun f acc xs -> match xs with
    | [] -> acc
    | head :: rest -> reduce f (f acc head) rest
;;

(* example *)
reduce (fun x y -> x + y) 0 [2;4;5;5];; (* == 16 *)

let reverse : 'a list -> 'a list = fun mylist ->
  reduce (fun acc a -> a :: acc) [] mylist
;;
reverse [1;3;2];;
(* - : int list = [2; 3; 1] *)


let max_of_list : int list -> int =
  fun int_list -> reduce max 0 int_list
;;

let largest_in : string list -> int =
  fun str_list ->
    let len_list = List.map String.length str_list in
    max_of_list len_list
;;

let string_of_int_set : int list -> string = function
  | [] -> ""
  | [n] -> "{" ^ string_of_int n ^ "}"
  | first :: rest ->
    let str_list = map string_of_int rest in
    let middle = reduce
        (fun a b -> a ^ "," ^ b)
        (string_of_int first)
        str_list
    in
    "{" ^ middle ^ "}"
;;

map string_of_int_set [[];[1;3];[1]];;
(* - : string list = [""; "{1,3}"; "{1}"] *)


(* interval d'entiers n tels que a <= n <= b, donc bornes incluses *)
let rec interval a b =
  if a > b then [] else
  let rec go n acc = 
     if a = n
      then (a :: acc)
      else go (n-1) (n :: acc)
  in
  go b []
;;


let table_automate afn =
  let sigma = afn.sigmaN in
  let sigma_row = "" :: "" :: map string_of_char sigma
  and make_state_row state_num =
    let state = afn.eN state_num in
    let str_state = string_of_int state_num in
    let first_col =
      if contains afn.initN state_num
      then "=>"
      else ""
    and second_col =
      if state.acceptN
      then "(" ^ str_state ^ ")"
      else str_state
    and rest = 
        map (fun letter ->
          let exploration = explore afn letter state_num
          in string_of_int_set exploration
        ) sigma
    in first_col :: second_col :: rest
  in
  let all_states = interval 1 afn.nN in
  let states_rows = map make_state_row all_states in
  sigma_row :: states_rows
;;

let rec zip : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list =
  fun f alist blist -> match (alist, blist) with
  | ([],_) -> []
  | (_,[]) -> []
  | (a::arest, b::brest) -> (f a b) :: (zip f arest brest)
;;

let get_columns : 'a list list -> 'a list list =
  fun table -> match reverse table with
  | [] -> []
  | last :: rest -> 
    let bottom = map (fun x -> [x]) last in
    reduce (fun acc row ->
      zip (fun cell col -> cell :: col) row acc) bottom rest
  ;;
  
get_columns [[1;2;3];[4;5;6];[7;8;9]];;
(* - : int list list = [[1; 4; 7]; [2; 5; 8]; [3; 6; 9]] *)

let format_table : int -> string list list -> string list list =
  fun padding table ->
    let columns = get_columns table in
    let spacing n = String.make n ' ' in
    let pad_string size str =
      let strlen = String.length str in
      let difference = size - strlen in
      let half = difference / 2 in
      let other_half = difference - half in
      (spacing half) ^ str ^ (spacing other_half)
    in
    let pad_column : string list -> string list =
      fun column ->
        let max_width = largest_in column + padding in
        map (pad_string max_width) column
    in
    get_columns (map pad_column columns)
      (* ^ reverse the table back *)
  ;;
  
let string_of_table : string -> int -> string list list -> string =
  fun separator padding table ->
  let nice_table = format_table padding table in
  let lines =
    map (
      fun cells ->
        (reduce
          (fun line str ->
            line ^ separator ^ str)
          ""
          cells)
        ^ separator
    )
    nice_table
  in
  match lines with
  | [] -> ""
  | first_line :: other_lines ->
    reduce
      (fun lines line -> lines ^ "\n" ^ line)
      first_line
      other_lines
;;
    
let print_table table =
  print_endline ((string_of_table "|" 2 table))
  ;;
  
let print_automaton afn = print_table (table_automate afn) ;;
    
(* td 5 exo 4  *)
let afnE4 = {sigmaN= ['a';'b';'E'] ; nN = 4; initN = [1] ; 
      eN = function 
          1 -> {acceptN = false ;
              tN = function 
                 'a'->[2]}
        |2 -> {acceptN = false ;
              tN = function 
                 'E'->[3] }
        |3 -> {acceptN = true ;
              tN = function 
               'b'->[2] | 'E' -> [4] }
        |4 -> {acceptN = false ;
              tN = function
               'a'->[4] | 'E' -> [2] }
    };;

print_automaton afnE4;;
(*
|    |     |  a  |  b  |  E  |
| => |  1  | {2} |     |     |
|    |  2  |     |     | {3} |
|    | (3) |     | {2} | {4} |
|    |  4  | {4} |     | {2} |
*)

let afn4 = elim_eps 'E' afnE4;;
print_automaton afn4;;
(*
|    |     |  a  |  b  |
| => |  1  | {2} |     |
|    | (2) | {4} | {2} |
|    | (3) | {4} | {2} |
|    | (4) | {4} | {2} |
*)

(* td5 exo 6 *)
let afnE6 = {sigmaN= ['a';'b';'E'] ; nN = 4; initN = [1] ; 
      eN = function 
        |1 -> {acceptN = false ;
              tN = function 
                 'a'->[1;2]}
        |2 -> {acceptN = false ;
              tN = function 
                 'b' -> [2;1] | 'E'-> [3;4] }
        |3 -> {acceptN = false ;
              tN = function 
                  'a'->[3] | 'b' -> [4;3] | 'E' -> [1] }
        |4 -> {acceptN = true ;
              tN = function
               'a'->[3] | 'b' -> [2] }
    };;

print_automaton afnE6;;
(* 
|    |     |   a   |   b   |   E   |
| => |  1  | {1,2} |       |       |
|    |  2  |       | {2,1} | {3,4} |
|    |  3  |  {3}  | {4,3} |  {1}  |
|    | (4) |  {3}  |  {2}  |       |
 *)
 
print_automaton (elim_eps 'E' afnE6);;
(* 
|    |     |    a    |     b     |
| => |  1  |  {1,2}  |           |
|    | (2) | {3,1,2} | {1,2,4,3} |
|    |  3  | {3,1,2} |   {4,3}   |
|    | (4) |   {3}   |    {2}    |
*)
 