let print_afn a=let m,j,si=List.map,String.length,string_of_int in
let ii y x=List.exists((=)x)y in let rec iv  a b=let rec f r n=if a>n
then r else f(n::r)(n-1)in f[]b in let rec z f r t=match(r,t)with|([]
,_)->[]|(_,[])->[]|(x::r,y::t)->f x y::z f r t in let u f d=function|
[]->d|x::r->List.fold_left f x r in let tt q l=try q.tN l with|
Match_failure _->[]in let sI i=match m si i with|[]->""|[s]->s|o->"{"
^u(fun a b->a^","^b)""o^"}"in let k a=let sG=a.sigmaN in(""::""::m(
String.make 1)sG)::m(fun h->(if(ii a.initN)h then"=>"else"")::(let s=
si h in if(a.eN h).acceptN then"("^s^")"else s)::(m((fun q c->sI(tt
q c))(a.eN h))sG))(iv 1 a.nN)in let v t=let p n=String.make n ' ' in
m(z(fun y s->let d=(y+2)-j s in let g=d/2 in(p g)^s^(p(d-g)))(u(z max
)[](m(m j)t)))t in print_endline(u(fun a b->a^"|\n"^b)""(m(u(fun
a b->a^"|"^b)"")(v(k a)))^"|") 
in
print_afn afnE6;;
