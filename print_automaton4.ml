(* nathanael bayard - L3 theorie des langages *)

type etatN = {
	acceptN : bool;
	tN : char -> int list
} ;;

type afn = {
	sigmaN: char list;
    nN: int;
    initN: int list;
    eN : int -> etatN
} ;; 


(* version compressee, plus facile a copier-coller : *)
let print_automaton a=let m,j,si=List.map,String.length,string_of_int
in let ii y x=List.exists((=)x)y in let rec iv  a b=let rec f r n=if
a>n then r else f(n::r)(n-1)in f[]b in let rec z f r t=match(r,t)with
|([],_)->[]|(_,[])->[]|(x::r,y::t)->f x y::z f r t in let u f d=
function|[]->d|x::r->List.fold_left f x r in let tt q l=try q.tN l
with|Match_failure _->[]in let sI i=match m si i with|[]->""|[s]->s|o
->"{"^u(fun a b->a^","^b)""o^"}"in let k a=let sG=a.sigmaN in(""::""
::m(String.make 1)sG)::m(fun h->(if(ii a.initN)h then"=>"else"")::(
let s=si h in if(a.eN h).acceptN then"("^s^")"else s)::(m((fun q c->
sI(tt q c))(a.eN h))sG))(iv 1 a.nN)in let v t=let p n=String.make n
' ' in m(z(fun y s->let d=(y+2)-j s in let g=d/2 in(p g)^s^(p(d-g)))(
u(z max)[](m(m j)t)))t in print_endline(u(fun a b->a^"|\n"^b)""(m(u(
fun a b->a^"|"^b)"")(v(k a)))^"|") ;;


(* version lisible; la fonction est definie en une seule
declaration. *)
let print_automaton : afn -> unit =
  let map = List.map in
  let string_of_char, strlen = String.make 1, String.length in
  (* x est-il element de la liste ys : *)
  let is_in ys x = List.exists ((=) x) ys in
  (* interval a b === [a; a+1; ...; b-1; b] *)
  let rec interval : int -> int -> int list = fun a b ->
    let rec fill result n =
      if a > n then result
      else fill (n :: result) (n-1) in
    fill [] b in
  (* zip f [a;b;c] [x;y;z] === [f a x; f b y; f c z] *)
  let rec zip
  : 'x 'y 'z. ('x->'y->'z) -> 'x list -> 'y list -> 'z list =
    fun f xs ys -> match (xs, ys) with
    | ([],_) -> []   (*  *)   | (_,[]) -> []
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
    fun state letter -> try state.tN letter with
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
    fun table -> fuse (zip max) [] (map (map strlen) table) in
  (* transforme un AFN en un tableau de strings representant *)
  (* la table des transitions de l'AFN, ses etats initiaux, etc : *)
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


(* exemple *)
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

print_automaton an1;; (*
    |     | a |   b   |
 => |  1  | 2 |       |
    | (2) | 2 | {2,3} |
    |  3  | 2 |   3   |
- : unit = () *)
