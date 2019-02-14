let print_automaton : afn -> unit =
  let map = List.map in
  let string_of_char, strlen = String.make 1, String.length in
  (* x est-il element de la liste ys : *)
  let is_in ys x = List.exists ((=) x) ys in
  (* interval a b === [a; a+1; ...; b-1; b] *)
  let rec interval : int -> int -> int list = fun a b ->
    let rec fill result n = if a > n
      then result
      else fill (n :: result) (n-1) in
    fill [] b in
  (* zip f [a;b;c] [x;y;z] === [f a x; f b y; f c z] *)
  let rec zip
  : 'x 'y 'z. ('x->'y->'z) -> 'x list -> 'y list -> 'z list =
    fun f xs ys -> match (xs, ys) with
    | ([],_) -> []
    | (_,[]) -> []
    | (x::xs, y::ys) -> f x y :: zip f xs ys in
  (* fuse: fusionne les elements d'une liste en une seule valeur *)
  (* fuse (+) 0 [1;42;100] === 143 *)
  (* fuse (^) "" ["hello ", "world"] === "hello world" *)
  (* fuse f def [a;b;c;d] === f (f (f a b) c) d *)
  (* fuse f def [] === def *)
  (* fuse f def [a] === a *)
  let fuse : 'x. ('x -> 'x -> 'x) -> 'x -> 'x list -> 'x =
    fun f default_value -> function
    | [] -> default_value 
    | x :: rest -> List.fold_left f x rest in
  (* etats accessibles a partir d'un etat en lisant une lettre : *)
  let transits : etatN -> char -> int list =
    fun state letter ->
      try state.tN letter with
      | Match_failure _ -> [] in
  (* ensemble d'entiers --> chaine de caracteres :
    [] --> "" ;; [1] --> "1" ;; [2;3] --> "{2,3}" *)
  let string_of_ints : int list -> string =
    fun ints -> match (map string_of_int ints) with
    | [] -> ""
    | [str] -> str
    | strlist -> let with_comma a b = a ^ "," ^ b
      in "{" ^ (fuse with_comma "" strlist) ^ "}" in
  (* donne le max de la taille des strings dans un tableau *)
  (* pour chaque colonne de celui-ci : *)
  let get_column_sizes : string list list -> int list =
    fun table -> fuse (zip max) []
      (map (map strlen) table) in
  let table_of_automaton : afn -> string list list =
    fun afn ->
    let sigma = afn.sigmaN in
    let sigma_row = "" :: "" :: map string_of_char sigma in
    let str_transits state c =
      string_of_ints (transits state c) in
    let make_state_row state_num =
      let col_one =
        if (is_in afn.initN) state_num then "=>" else "" in
      let col_two =
        let s = string_of_int state_num in
        if (afn.eN state_num).acceptN
        then "(" ^ s ^ ")" else s in
      let state_transits =
        map (str_transits (afn.eN state_num)) sigma in
      col_one :: col_two :: state_transits in
    let all_states = interval 1 afn.nN in
    let states_rows = map make_state_row all_states in
    sigma_row :: states_rows in
  (* bourre les cases de la table avec des espaces pour *)
  (* avoir un affichage regulier : *)
  let format_table
  : int -> string list list -> string list list =
    fun margin table ->
      let column_sizes = get_column_sizes table in
      let spaces n = String.make n ' ' in
      let pad_string size str =
        let diff = (size + margin) - strlen str in
        let left = diff / 2 in let right = diff - left in
        (spaces left) ^ str ^ (spaces right) in
      map (zip pad_string column_sizes) table in
  let string_of_table
  : string -> int -> string list list -> string =
    fun sep margin table ->
      let nice_table = format_table margin table in
      let with_sep s1 s2 = s1 ^ sep ^ s2 in
      let lines = map (fuse with_sep "") nice_table in
      let with_newline l1 l2 = l1 ^ sep ^ "\n" ^ l2 in
      fuse with_newline "" lines ^ sep in
  fun afn -> let table = table_of_automaton afn in
    print_endline (string_of_table "|" 2 table)
;;

let f : 'a -> 'a list = fun x -> [x];;
let a = f 3;;
let b = f false;;

let c =
  let f : 'a -> 'a list = fun x -> [x] in
  (f 3, f true);;
  let 

let map = List.map;;
let string_of_char = String.make 1 ;;
let strlen = String.length;;

let rec is_in ls x = match ls with
    | [] -> false
    | y :: ys -> (y = x) || is_in ys x
;;

let is_in ys x = List.exists ((=) x) ys ;;
let is_in = fold_left (
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

let rec zip : ('x -> 'y -> 'z) -> 'x list -> 'y list -> 'z list =
  fun f xs ys -> match (xs, ys) with
  | ([],_) -> []
  | (_,[]) -> []
  | (x::xs, y::ys) -> f x y :: zip f xs ys
;;

let fuse : ('x -> 'x -> 'x) -> 'x -> 'x list -> 'x =
  fun f default_value -> function
  | [] -> default_value 
  | x :: rest -> List.fold_left f x rest
;;

let rec interval : int -> int -> int list = fun a b ->
  let rec fill result n = if a > n
    then result
    else fill (n :: result) (n-1)
  in fill [] b
;;

let transits : etatN -> char -> int list =
  fun state letter ->
    try state.tN letter with
    | Match_failure _ -> []
;;

(* [] --> "" ;; [43] --> "43" ;; [3,14] --> "{3,14}" *)
let string_of_ints : int list -> string =
  fun ints -> match (map string_of_int ints) with
  | [] -> ""
  | [str] -> str
  | strlist -> let with_comma a b = a ^ "," ^ b
    in "{" ^ (fuse with_comma "" strlist) ^ "}"
;;

let string_of_ints : int list -> string = fun ints ->
  let strs = map string_of_int ints
  and with_comma a b = a ^ "," ^ b 
  in "{" ^ (fuse with_comma "" strs) ^ "}"
;;


let table_of_automaton : afn -> string list list = fun afn ->
  let sigma = afn.sigmaN in
  let sigma_row = "" :: "" :: map string_of_char sigma
  and str_transits state c = string_of_ints (transits state c) in
  let make_state_row state_num =
    let col_one =
      if (is_in afn.initN) state_num then "=>" else ""
    and col_two =
      let s = string_of_int state_num in
      if (afn.eN state_num).acceptN
      then "(" ^ s ^ ")" else s
    and state_transits =
      map (str_transits (afn.eN state_num)) sigma
    in col_one :: col_two :: state_transits
  in
  let all_states = interval 1 afn.nN in
  let states_rows = map make_state_row all_states in
  sigma_row :: states_rows
;;

let get_column_sizes
: string list list -> int list = fun table ->
  fuse (zip max) [] (map (map strlen) table)
;;

let format_table : int -> string list list -> string list list =
  fun margin table ->
    let column_sizes = get_column_sizes table in
    let spaces n = String.make n ' ' in
    let pad_string size str =
      let diff = (size + margin) - strlen str in
      let left = diff / 2 in let right = diff - left in
      (spaces left) ^ str ^ (spaces right)
    in
    map (zip pad_string column_sizes) table
  ;;

let string_of_table : string -> int -> string list list -> string =
  fun sep margin table ->
    let nice_table = format_table margin table
    and with_sep s1 s2 = s1 ^ sep ^ s2 in
    let lines = map (fuse with_sep "") nice_table
    and with_newline l1 l2 = l1 ^ "|\n" ^ l2 in
    fuse with_newline "" lines ^ "|"
;;

let print_table table =
  print_endline ((string_of_table "|" 2 table))
  ;;

let print_automaton afn = print_table (table_of_automaton afn) ;;



(* td 5 exo 4  *)
let afnE4 = {sigmaN= ['a';'b';'E'] ; nN = 4; initN = [1;4] ; 
      eN = function 
          1 -> {acceptN = false ;
              tN = function 
                 'a'->[2]}
        |2 -> {acceptN = true ;
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


