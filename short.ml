let print_automaton=let m,sK,w,q,z=List.map,String.make,String.length,"",
List.zip_with in let k f d xs=match xs with |[]->d |x::f->List.fold_left f x f
in let rec iv a b=let rec d r n=if a>n then r else d(n::r)(n-1)in d[]n and tt
st ll=try st.tN ll with |Match_failure _->[]and sO ii=let ss=m string_of_int
ii and wc a b=a^","^b in "{"^(k wc q ss)^"}" and gC h=k(z max)[](m(m w)h)in let
tba j=let sH=j.sigmaN in let sG=q::q::m(sK 1)sH and sstt g ll=sO(tt(j.eN g)ll)
and iI= is_in j.initN and ks g=let cc1=if iI g then "=>" else q and cc2=let s=
string_of_int g in if(j.eN g).acceptN then "("^s^")" else s and st_tt=m(sstt g)
in cc1::cc2::st_tt sH in let al=iv 1 j.nN in let sR=m ks al in sG::sR and ftB e
h=let cZ=gC h in let spc n=sK n ' ' in let ps sz s=let o=w s in let d=(sz+e)-o
in let l=d/2 in let r=d-l in(spc l)^s^(spc r)in m(z ps cZ)h in let sJ p e h=let
nh=fh e h and wsP u v=u^p^v in let lL = m(fun rw->k wsP q rw)nh and wnl l1
l2=l1^"\n" l2 in k wnl q lL in fun j->in print_endline(sJ "|" 2(tba j));;