
		
type etat = {accept : bool ; t : char -> int} ;;		(* type etat = { accept : bool; t : char -> int; } *)

(*Automate fini déterministe *)
type afd = {sigma : char list ; nQ : int ; init : int ; e : int -> etat} ;;

(* L'automate exemple a1 *)
let a1 = {
	sigma= ['a';'b'];
	nQ = 3;
	init = 1; 
	e = function	
	    1 -> {accept = false ;
		      t = function 
			       'a'->2
				   |'b'-> 1  }
		|2 -> {accept = false ;
		      t = function 
			       'a'->2
				   |'b'-> 3  }		   
		|3 -> {accept = true ;
		      t = function 
			       'a'->3
				   |'b'->3   }		   				
};;
(* automate exemple a2 *)
let a2 = {sigma= ['a';'b'] ; nQ = 3; init = 1 ; 
            e = function    
                1 -> {accept = false ;
                      t = function 
                           'a'->2 }
                |2 -> {accept = false ;
                      t = function 
                           'a'->2
                           |'b'-> 3  }         
                |3 -> {accept = true ;
                      t = function 
                           'a'->3
                           |'b'->3   }                      
        };;


let f = (function [] -> 3) in 
    try f [1] with Match_failure _ -> 4;;

let transition state letter =
    try Some (state.t letter) with
    | Match_failure _ -> None
;;

let rec read afd word state_num =
    let state = afd.e state_num
    in
	match word with
	| "" -> Some state_num
	| w ->
        let l = w.[0] and tail = String.sub w 1 (String.length w - 1)
        in (match transition state l with
        | Some next_state_num -> read afd tail next_state_num
        | None -> None)
    ;;

let rec contains ls x = match ls with
    | [] -> false
    | y :: ys -> (y = x) || contains ys x
;;

let accepte afd word : bool =
    match read afd word afd.init with
    | None -> false
    | Some state_num -> (afd.e state_num).accept
;;

let ac1 = accepte a1;;
List.map ac1 ["abba";"bbaaa";"bbaaba";"ba";"ab";""];;
accepte a2 "babb";;



let empty_transition =
    function _ -> match [[]] with | [] -> 42
;;
(* automate a*.b *)
let astarb = {sigma= ['a';'b'] ; nQ = 2; init = 1 ; 
            e = function    
                1 -> {accept = false ;
                      t = function 
                           'a'->1 
                           |'b'->2 }
                |2 -> {accept = true ;
                      t = empty_transition }         
        };;
List.map (accepte astarb) ["abba";"abb";"b";"aaaaab";"a";""];;

