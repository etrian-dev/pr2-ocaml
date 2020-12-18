(*Apre il modulo dell'interprete*)
open Interprete;;

(* =================  TESTS  ================= *)
(*empty env*)
print_endline "*** Dichiaro l'ambiente vuoto ***";;
let env0 = emptyenv Unbound;;

print_endline "*** Dichiaro la funzione inc1 che incrementa l'argomento (intero) di 1 ***";;
let inc1 = Fun("y", Sum(Den "y", Eint 1));;
print_endline "*** Dichiaro la funzione che testa se l'argomento (stringa) è \"Test\" ***";;
let isTest = Fun("s", Eq(Den "s", Estring("Test")));;

(*La chiama con arg. 3 (valuta a partire dall'ambiente vuoto) => Int 4*)
print_endline "*** Chiamo inc1 con argomento 3 => Int 4 ***";;
print_exp (FunCall(inc1, Eint 3)) env0;;

(*Modifica l'ambiente con il bind y = 5 e poi chiama la funzione => 6*)
print_endline "*** Estendo env0 con bind x=5 => env1 ***";;
let env1 = bind env0 "x" (Int(5));;
print_endline "*** Funcall(x -> x+1, x) => Int 6 ***";;
print_exp (FunCall(inc1, Den "x")) env1;;

(*dichiara le stringhe "Hello, " e "Ocaml", poi le concatena*)
print_endline "Concateno le stringhe \"Test\" ed \"OcaML\"";;
let hi = Estring("Test");;
let oc = Estring("OcaML");;
(*Le concatena con l'operazione Concat*)
print_exp (Concat(hi, oc)) env0;;

(*Test operazioni sui Set*)
print_endline "*** Testing dei Set e relative operazioni ***";;
print_endline "*** Dichiaro un set per ogni combinazione di costruttori e tipi ***";;
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
let lstr = Set([Estring("A");Estring("b");Estring("C");Estring("D");Estring("E");Estring("OcaML")], Estring("string"));;

let setlist = [eint; ebool; estr; sint; sbool; sstr; lint; lbool; lstr];;

(*Valuto tutti i set*)
print_endline "*** Valuto i Set, ottenendo i denotabili SetVal corrispondenti ***";;
print_exp eint env0;;
print_exp ebool env0;;
print_exp estr env0;;
print_exp sint env0;;
print_exp sbool env0;;
print_exp sstr env0;;
print_exp lint env0;;
print_exp lbool env0;;
print_exp lstr env0;;

(*  Testo ciascun set dichiarato con la funzione isEmpty e conto il numero di set vuoti
 *  Questo numero deve risultare 3
 *)
print_endline "*** Conto numero di set vuoti tra quelli definiti => 3 ***";;
let rec count_empty ls acc = match ls with
  | [] -> acc
  | hd::tl -> 
    if (eval (IsEmpty(hd)) env0) = Bool(true)
    then count_empty tl acc+1
    else count_empty tl acc
in Printf.printf "numero set vuoti: %d\n" (count_empty setlist 0);;

(*Cardinalità di lint => Int 3*)
print_endline "*** Cardinalità di lint => 3 ***";;
print_exp (Size(lint)) env0;;
(*Cardinalità di lint1 => Int 5*)
print_endline "*** Cardinalità di lint1 => 5 ***";;
(*Cardinalità di sstr => Int 1*)
print_exp (Size(lint1)) env0;;
print_endline "*** Cardinalità di sstr => 1 ***";;
print_exp (Size(sstr)) env0;;
(*Cardinalità di ebool => Int 0*)
print_endline "*** Cardinalità di ebool => 0 ***";;
print_exp (Size(ebool)) env0;;

(*Inserisco la stringa "Ocaml" nel set lstr*)
print_endline "*** Inserisco la stringa \"OcaML\" nel set lstr => nessuna modifica + warning ***";;
print_exp (Insert(lstr, oc)) env0;;
print_endline "*** Inserisco la stringa \"Hello, \" nel set lstr => ok ***";;
print_exp (Insert(lstr, hi)) env0;;

print_endline "*** Definisco una Fun che ritorna Bool(true) sse l'argomento x != Int 0 ***";;
let notZero = Fun("x", Not(Eq(Den "x", Eint(0))));;

print_endline "*** Map(add1, lint) => incrementa di 1 tutti gli elementi di lint ***";;
let lint_plus_1 = Map(inc1, lint);;
print_exp (lint_plus_1) env0;;

print_endline "*** Unisco i set lint_plus_1 e lint1 ***";;
let union = Merge(lint_plus_1, lint1);;
print_exp union env0;;

print_endline "*** Filtro union secondo la funzione notZero => Elimina Int 0 dal Set union ***";;
print_exp (Filter((notZero, union))) env0;;
print_endline "*** Considero la funzione zero (negata della precedente)\n\te filtro lo stesso set => SetVal([Int 0], \"int\") ***";;
let zero = Fun("x", Eq(Den "x", Eint(0)));;
print_exp (Filter((zero, union))) env0;;