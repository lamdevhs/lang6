(* Bibliothèque sur les listes et les chaînes de caractères en fin de page *)

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
"ecrase" verticalement. c'est aussi la liste de lecture de l'automate
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
  [[[1]; [1; 2]; [3; 5]; [2; 1; 5]]; <- arbre de lecture ecrase de abc
  
  [[1]; []; []; []; []; []]; <--- arbre de lecture ecrase de baabc
  
  [[1]; [1; 2]; [1; 2]; [3; 5]; [2; 1; 5];
  [3; 5]; [2; 1; 5]; [1; 2]; [3; 5]]; <--- arbre de lecture ecrase de baabc
  
   [[1]; [1; 2]; [3; 5]; [2; 1; 5]; [1; 2];
   [3; 5]; [2; 1; 5]; [2; 4]; []]] <--- arbre de lecture ecrase de baabc
 *)
let results = List.map (acceptN an2) words ;;

