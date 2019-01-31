
(*		TP n°2 - Automate fini déterministe			*)
		
type etat = {accept : bool ; t : char -> int} ;;		(* type etat = { accept : bool; t : char -> int; } *)


type afd = {sigma : char list ; nQ : int ; init : int ; e : int -> etat} ;;

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
let transition state letter =
    try Some (state.t letter) with
    | Match_failure _ -> None
;;
		
let complete_state afd state puits_refusant =
	let rec new_transitions letters = match letters with
		| [] -> state.t
		| l :: rest ->
			if transition state l = None
			then (function letter ->
				if letter = l then puits_refusant else (new_transitions rest) letter)
			else new_transitions rest

	in let new_t = new_transitions afd.sigma
	in let new_state = {accept = state.accept; t = new_transitions afd.sigma}
	in let has_changed = (new_t != state.t)
	in
	if has_changed then new_state else state
;;

let test = (function n ->
	let old = a2.e n in
	let newst = complete_state a2 old 4 in
	(transition old 'a',transition old 'b',
	transition newst 'a', transition newst 'b',
	old == newst));;
test 2;;
test 1;;
test 3;;


let complete afd =
	let puits_refusant = afd.nQ + 1 in
	let rec go n =
		if n = 0 then afd.e else
		let old_state = afd.e n in
		let new_state = complete_state afd old_state puits_refusant
		in
		if old_state == new_state
		then go (n-1)
		else
			(let new_afde = go (n-1) in
			function k -> if k = n then new_state else new_afde k)
	in
	let new_afde = go afd.nQ in
	let has_changed = (new_afde != afd.e) in
	if not has_changed then afd
	else
	let etat_puits_refusant = {accept = false; t = (function _ -> puits_refusant)}
	in
	{
		sigma = afd.sigma;
		nQ = puits_refusant;
		init = afd.init;
		e = (function k ->
			if k = puits_refusant
			then etat_puits_refusant
			else new_afde k)
	}
	;;

let a3 = complete a2;;

List.map
	(function n ->
		let state3 = a3.e n and state2 = a2.e n in
		List.map 
			(function letter ->
				(n, letter, transition state2 letter, transition state3 letter))
			a3.sigma)
	[1;2;3];;
let empty_transition =
    function _ -> match [[]] with | [] -> 42
;;
let astarb = {sigma= ['a';'b'] ; nQ = 2; init = 1 ; 
            e = function    
                1 -> {accept = false ;
                      t = function 
                           'a'->1 
                           |'b'->2 }
                |2 -> {accept = true ;
                      t = empty_transition }         
        };;
let completed_astarb = complete astarb;;

List.map
	(function n ->
		let state3 = astarb.e n and state2 = completed_astarb.e n in
		List.map 
			(function letter ->
				(n, letter, transition state2 letter, transition state3 letter))
			a3.sigma)
	[1;2];;