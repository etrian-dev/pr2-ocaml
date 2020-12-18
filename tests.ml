(* =================  TESTS  ================= *)
(*empty env*)
print_endline "dichiaro l'ambiente vuoto";;
let env0 = emptyenv Unbound;;

(*dichiara la funzione function y -> y+1*)
print_endline "dichiaro la funzione che incrementa gli interi di 1";;
let inc1 = Fun("y", Sum(Den "y", Eint 1));;

(*La chiama con arg. 3 (valuta a partire dall'ambiente vuoto) => Int 4*)
print_endline "La chiamo con argomento 3 => Int 4";;
eval (FunCall(inc1, Eint 3)) env0;;

(*Modifica l'ambiente con il bind y = 5 e poi chiama la funzione => 6*)
print_endline "estendo env0 con bind y=5";;
let env1 = bind env0 "y" (Int(5));;
print_endline "Funcall(y -> y+1, Int(5)) => Int 6";;
eval (FunCall(inc1, Den "y")) env1;;

(*dichiara le stringhe "Hello, " e "Ocaml"*)
print_endline "Concateno le stringhe \"Hello, \" ed \"Ocaml\"";;
let hi = Estring("Hello, ");;
let oc = Estring("Ocaml");;
(*Le concatena con l'operazione Concat*)
eval (Concat(hi, oc)) env0;;


print_endline "**** Testing dei Set e relative operazioni ****";;
print_endline "Dichiaro un set per ciascuna combinazione ammissibile di tipi e costruttori";;
(*empty sets*)
let eint = EmptySet(Estring("int"));;
let ebool = EmptySet(Estring("bool"));;
let estr = EmptySet(Estring("string"));;
(*singletons*)
let sint = Singleton(Eint(0), Estring("int"));;
let sbool = Singleton(Ebool(true), Estring("bool"));;
let sstr = Singleton(oc, Estring("string"));;
(*sets initialized with a list*)
let lint = Set([Eint(10);Eint(10);Eint(-1);Eint(6)], Estring("int"));;
let lint1 = Set([Eint(6);Eint(4);Eint(3);Eint(-1);Eint(18)], Estring("int"));;
let lbool = Set([Ebool(true);Ebool(false);Ebool(false);Ebool(true);Ebool(true);Ebool(false);Ebool(false)], Estring("bool"));;
let lstr = Set([Estring("A");Estring("b");Estring("C");Estring("D");Estring("E");Estring("Ocaml")], Estring("string"));;

let setlist = [eint; ebool; estr; sint; sbool; sstr; lint; lbool; lstr];;

(*Valuto tutti i set*)
print_endline "Valuto tutti i Set dichiarati ai corrispondenti SetVal";;
eval eint env0;;
eval ebool env0;;
eval estr env0;;
eval sint env0;;
eval sbool env0;;
eval sstr env0;;
eval lint env0;;
eval lbool env0;;
eval lstr env0;;

(*  Testo ciascun set dichiarato con la funzione isEmpty e conto il numero di set vuoti
 *  Questo numero deve risultare 3
 *)
print_endline "Conto numero di set vuoti tra quelli definiti => 3";;
let rec count_empty ls acc = match ls with
  | [] -> acc
  | hd::tl -> 
    if (eval (IsEmpty(hd)) env0) = Bool(true)
    then count_empty tl acc+1
    else count_empty tl acc
in count_empty setlist 0;;

(*Cardinalità di lint => Int 3*)
print_endline "Cardinalità di lint";;
eval (Size(lint)) env0;;
(*Cardinalità di lint1 => Int 5*)
print_endline "Cardinalità di sstr";;
eval (Size(sstr)) env0;;
(*Cardinalità di lint => Int 3*)
print_endline "Cardinalità di ebool";;
eval (Size(ebool)) env0;;



(*Inserisco la stringa "Ocaml" nel set lstr*)
print_endline "Inserisco la stringa \"Ocaml\" nel set lstr => warning";;
eval (Insert(lstr, oc)) env0;;
print_endline "Inserisco la stringa \"Hello, \" nel set lstr => ok";;
eval (Insert(lstr, hi)) env0;;

print_endline "Definisco la funzione che ritorna Bool(true) sse l'argomento x != Int(0)";;
let notZero = Fun("x", Not(Eq(Den "x", Eint(0))));;

print_endline "Applico la funzione add1 sul Set lint => incrementa di 1 tutti gli elementi";;
let incrSet = Map(add1, lint);;
eval (incrSet) env0;;
print_endline "Unisco i set incrSet e lint1";;
let union = Merge(incrSet, lint1);;
eval union env0;;
print_endline "Filtro secondo la funzione notZero => Elimina Int 0 dal Set";;
eval (Filter((notZero, union))) env0;;
print_endline "Poi nego la funzione e filtro lo stesso set => SetVal([Int 0], \"int\")";;
let zero = Fun("x", Eq(Den "x", Eint(0)));;
eval (Filter((zero, union))) env0;;