let sigma = ['a';'b'];;

let w = ['a';'b';'b';'a'];;

let rec conversion str =
  let ln = String.length str in
  if ln = 0
    then []
    else let tail = (String.sub str 1 (ln-1)) in
	str.[0] :: (conversion tail)
  ;;

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