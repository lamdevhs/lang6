let sigma = ['a';'b'];;

let w = ['a';'b';'b';'a'];;

let rec conversion str =
  let ln = String.length str in
  if ln = 0
    then []
    else let tail = (String.sub str 1 (ln-1)) in
    str.[0] :: (conversion tail)
  ;;
let conv = conversion;;
conversion "abcd";;

let rec teste_lettre (lettre : 'a) (alph : 'a list) = match alph with
  | [] -> false
  | head :: tail -> (head = lettre) || teste_lettre lettre tail
  ;;

teste_lettre 'a' sigma;;
teste_lettre 'z' sigma;;

let rec teste_mot (alph : 'a list) = function
  | [] -> true
  | letter :: tail -> (teste_lettre letter alph) && (teste_mot alph tail)
  ;;

teste_mot sigma w;;

let v = conversion "abc";;
teste_mot sigma v ;;

let rec egalite (mots : ('a list * 'a list)) =
  match mots with
    | ([],[]) -> true
    | (_,[]) -> false
    | ([], _) -> false
    | (x :: xs, y :: ys) -> (x = y) && (egalite (xs,ys)) ;;
egalite (w,w) ;;
egalite (w,['a'; 'b'; 'b'; 'a'; 'a']) ;;

let concaten v w = v @ w ;;

let rec puissance w n =
  if n = 0 then []
  else
    let half = n / 2
    and double = concaten w w
    in
    if n mod 2 = 0
      then puissance double half
      else concaten w (puissance double half)
  ;;
List.map (puissance w) [0;1;2;3];;
puissance (conversion "ab") 11;;

exception PasPrefixe;;

let rec prefixe_reste ((pref,w) : 'a list * 'a list) =
  match (pref, w) with
  | ([],w) -> w
  | (_,[]) -> raise PasPrefixe
  | (p :: pr, l :: tail) ->
    if p = l
      then prefixe_reste (pr, tail)
      else raise PasPrefixe
  ;;
prefixe_reste (w, w @ ['a']) ;;
prefixe_reste (w, w) ;;
prefixe_reste (w, ['a'] @ w @ w) ;;
prefixe_reste (w, ['a']) ;;

let rec est_puiss = function
  | (w, []) -> true
  | (w, v) ->
    (try est_puiss (w, prefixe_reste (w, v))
    with PasPrefixe -> false)
  ;;
est_puiss (w, ['a'; 'b'; 'b'; 'a'; 'a']);;
est_puiss (w, puissance w 42);;
est_puiss (w, []);;

(* mots conjugues *)

type 'a option = Some of 'a | None ;;

let rec extract m l = match (m,l) with
    | (m, []) -> []
    | (0, x :: tail) -> [x]
    | (m, x :: tail) -> x :: (extract (m-1) tail)
;;
let rec tranche n m l = match (n,m,l) with
    | (0,m,l) -> let res = extract m l in
        if List.length res = m + 1 then Some res else None
    | (n, m, []) -> None
    | (n, m, x :: tail) -> tranche (n-1) (m-1) tail
;;
tranche 2 4 ['a';'b';'b';'a';'a'];;

let sont_conjugues u v =
    if List.length u <> List.length v then false
    else
    let ln = List.length u
    in
    if u = v then true
    else
    let rec one_test n =
        if n = -1 then false
        else
        let p = (ln-1) - n in
        let test =
            (tranche 0 n u = tranche p (ln-1) v)
            &&
            (tranche (n+1) (ln-1) u = tranche 0 (p-1) v)
        in test || one_test (n-1)
    in
    one_test (ln-1)
;;

sont_conjugues [1;2;3;4;5;6] [3;4;5;6;1;2];;
sont_conjugues [2;3;4;5] [3;4;5;2];;
sont_conjugues [3;4;5] [3;4;5];;
sont_conjugues [] [] ;;


let plus_long_pref l =
    let rev = List.rev l
    in
    let rec go (rev, l) =
        match (rev,l) with
        | ([], []) -> []
        | (x::xs, y::ys) ->
            if x = y
            then x :: go (xs, ys)
            else []
    in go (rev, l)
    ;;

plus_long_pref [] ;;
plus_long_pref [1;2] ;;
plus_long_pref [1;2;1] ;;
plus_long_pref [1;2;4;3;4;1;2;4] ;;

let plus_long_pref l =
    let ln = List.length l
    in
    let test n l =
        (tranche 0 n l) = (tranche (ln-1-n) (ln-1) l)
    in
    let rec go n acc =
        if (n+1)*2 > ln then acc
        else
        if test n l
        then go (n + 1) n
        else go (n + 1) acc
    in tranche 0 (go 0 (-1)) l
    ;;

(*langages*)

let l0 = [];;
let l1 = [[]];;
let l2 = List.map (function letter -> [letter]) sigma ;;
let l3 = [conv "baa"; conv "bab"; conv "bba"; conv "bbb"];;
let l4 = [conv "baa"; conv "aaa"; conv "aab"];;

let rec is_in w l1 = match l1 with
    | [] -> false
    | x :: rest -> if w = x then true else is_in w rest ;;

let rec somme l1 l2 = match l2 with
    | [] -> l1
    | x :: rest ->
        let cont = somme l1 rest in
        if is_in x l1 then cont else x :: cont ;;

somme l3 l4;;

let rec intersection l1 l2 = match l2 with
    | [] -> []
    | x :: rest ->
        let cont = intersection l1 rest in
        if is_in x l1 then x :: cont else cont ;;

intersection l3 l4;;

let rec concaten_mot_lang w lang =
    List.map (function u -> concaten w u) lang ;;

concaten_mot_lang ['a'] l2 ;;

let rec produit lan1 lan2 = match lan1 with
    | [] -> []
    | w :: rest ->
        let lan3 = concaten_mot_lang w lan2
        in somme lan3 (produit rest lan2)
    ;;

produit l2 l4;;
produit l0 l4;;
produit l1 l4;;

let rec puiss_lang lang n =
    if n == 0 then [[]]
    else produit lang (puiss_lang lang (n-1)) ;;

puiss_lang l2 2;;


type bexp
    = Epsilon
    | Char of char
    | Union of bexp * bexp
    | Concat of bexp * bexp
    | Star of bexp
;;

let a = Char 'a' and b = Char 'b';;
let re1 =
    Concat(Star (Union (Concat (a,b), b)), Concat (b, a))
;;
let re2 =
    Concat(b, Star (Concat(a, Union (Concat (a,b), b))));;

let rec string_of_bep bep =
    let oper sym e1 e2 = "(" ^ string_of_bep e1
        ^ sym ^ string_of_bep e2 ^ ")"
    in
    match bep with
    | Epsilon -> "'"
    | Char c -> String.make 1 c
    | Union (e1,e2) -> oper "+" e1 e2
    | Concat (e1,e2) -> string_of_bep e1 ^  string_of_bep e2
    | Star e -> "(" ^ string_of_bep e ^ ")*"
;;
string_of_bep re1;;
string_of_bep re2;;


type regexp
    = REpsilon
    | RChar of char
    | RUnion of regexp list
    | RConcat of regexp list
    | RStar of regexp
;;

let rec regexp_of_bexp bexp =
    let rec flatten_union u = match u with
        | Union (a, b) -> flatten_union a @ flatten_union b
        | otherwise -> [otherwise]
    in
    let rec flatten_concat k = match k with
        | Concat (a, b) -> flatten_concat a @ flatten_concat b
        | otherwise -> [otherwise]
    in
    match bexp with
    | Epsilon -> REpsilon
    | Char c -> RChar c
    | Union (a, b) ->
        let flat = flatten_union a @ flatten_union b
        in RUnion (List.map regexp_of_bexp flat)
    | Concat (a, b) ->
        let flat = flatten_concat a @ flatten_concat b
        in RConcat (List.map regexp_of_bexp flat)
    | Star s -> RStar (regexp_of_bexp s)
;;

let test =
    Concat(
        Union(a, Union (Star a, Union (a, b))),
        Concat(Star b,a));;
regexp_of_bexp test;;

let rec string_of_regexp reg = 
    let rec oper sym ls = match ls with
        | [x] -> string_of_regexp x ^ ")"
        | (x :: xs) -> string_of_regexp x ^ sym ^ oper sym xs
    in
    match reg with
    | REpsilon -> "'"
    | RChar c -> String.make 1 c
    | RUnion [] -> "{}"
    | RUnion ls -> "(" ^ oper "+" ls
    | RConcat [] -> "{any}"
    | RConcat ls -> "(" ^ oper "" ls
    | RStar (RChar c) -> String.make 1 c ^ "*"
    | RStar e -> "(" ^ string_of_regexp e ^ ")*"
;;
string_of_regexp (regexp_of_bexp test);;