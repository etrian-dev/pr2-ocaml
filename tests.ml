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

(*============== Test per Size ==============*)
print_endline "(*============== Test per IsEmtpy ==============*)";;
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

(*============== Test per Size ==============*)
print_endline "(*============== Test per Size ==============*)";;
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

(*============== Test per Contains ==============*)
print_endline "(*============== Test per Contains ==============*)";;
print_endline ("*** Int 6 ∊ "^((string_of_evT (eval lint env0)))^"? ***");;
print_exp (Contains(lint, Eint(6))) env0;;
print_endline ("*** String \"aaa\" ∊ "^(string_of_evT (eval lstr env0))^"? ***");;
print_exp (Contains(lstr, Estring("aaa"))) env0;;
print_endline ("*** Int 100 ∊ "^(string_of_evT (eval sint env0))^"? ***");;
print_exp (Contains(sint, Eint(100))) env0;;
print_endline ("*** Bool(true) ∊ "^(string_of_evT (eval ebool env0))^"? ***");;
print_exp (Contains(ebool, Ebool(true))) env0;;
print_endline ("*** \"test\" ∊ "^(string_of_evT (eval lint1 env0))^"? ***");;
print_exp (Contains(lint1, Estring("test"))) env0;;

(*============== Test per Insert ==============*)
print_endline "(*============== Test per Insert ==============*)";;
print_endline ("*** Inserisco la stringa \"Inserita\" nel set "^((string_of_evT (eval sstr env0)))^" ***");;
print_exp (Insert(sstr, Estring("Inserita"))) env0;;
print_endline ("*** Inserisco Bool(true) in "^((string_of_evT (eval ebool env0)))^" ***");;
print_exp (Insert(ebool, Ebool(true))) env0;;
print_endline ("*** Inserisco la stringa \"OcaML\" nel set "^((string_of_evT (eval sstr env0)))^" ***");;
print_exp (Insert(sstr, oc)) env0;;
print_endline ("*** Provo ad inserire la stringa \"errore di tipo\" in "^((string_of_evT (eval lint env0)))^" => Eccezione: errore di tipo ***");;
try print_exp (Insert(lint, Estring("errore di tipo"))) env0 with 
  | Failure(s) -> print_endline ("Caught "^s)
;;

(*============== Test per Remove ==============*)
print_endline "(*============== Test per Remove ==============*)";;
print_endline ("*** Rimuovo Int 3 e Int 18 da "^(string_of_evT (eval lint1 env0))^" ***");;
print_exp (Remove((Remove(lint1, Eint(3))), Eint(18))) env0;;
print_endline ("*** Rimuovo Int 5 da "^(string_of_evT (eval lint1 env0))^" ***");;
print_exp (Remove(lint1, Eint(5))) env0;;
print_endline ("*** Rimuovo \"str\" da "^(string_of_evT (eval estr env0))^" ***");;
print_exp (Remove(estr, Estring("str"))) env0;;
print_endline ("*** Provo a rimuovere \"str\" da "^(string_of_evT (eval lbool env0))^" => Eccezione: errore di tipo ***");;
try print_exp (Remove(lbool, Estring("str"))) env0 with
  | Failure(s) -> print_endline ("Caught "^s)
;;

(*============== Test per Subset ==============*)
print_endline "(*============== Test per Subset ==============*)";;
print_endline ("*** "^(string_of_evT (eval lint1 env0))^" ⊆ "^(string_of_evT (eval lint env0))^"? ***");;
print_exp (Subset(lint1, lint)) env0;;
print_endline ("*** "^(string_of_evT (eval (Remove(lint, Eint(10))) env0))^" ⊆ "^(string_of_evT (eval lint1 env0))^"? ***");;
print_exp (Subset((Remove(lint, Eint(10))), lint1)) env0;;
print_endline ("*** "^(string_of_evT (eval estr env0))^" ⊆ "^(string_of_evT (eval sstr env0))^"? ***");;
print_exp (Subset(estr, sstr)) env0;;
print_endline ("*** "^(string_of_evT (eval ebool env0))^" ⊆ "^(string_of_evT (eval ebool env0))^"? ***");;
print_exp (Subset(ebool, ebool)) env0;;
print_endline ("*** "^(string_of_evT (eval sint env0))^" ⊆ "^(string_of_evT (eval sbool env0))^"? ***");;
print_exp (Subset(sint, sbool)) env0;;

(*============== Test per SetMin/SetMax ==============*)
print_endline "(*============== Test per SetMin/SetMax ==============*)";;
print_endline ("*** minimo/massimo di "^(string_of_evT (eval lint env0))^" ***");;
print_exp (SetMin(lint)) env0;;
print_exp (SetMax(lint)) env0;;
print_endline ("*** minimo/massimo di "^(string_of_evT (eval sint env0))^" ***");;
print_exp (SetMin(sint)) env0;;
print_exp (SetMax(sint)) env0;;
print_endline ("*** minimo/massimo di "^(string_of_evT (eval eint env0))^" ***");;
print_exp (SetMin(eint)) env0;;
print_exp (SetMax(eint)) env0;;
print_endline ("*** minimo/massimo (ord. lessicografico) di "^(string_of_evT (eval lstr env0))^" ***");;
print_exp (SetMin(lstr)) env0;;
print_exp (SetMax(lstr)) env0;;
print_endline ("*** minimo/massimo (ord. lessicografico) di "^(string_of_evT (eval sstr env0))^" ***");;
print_exp (SetMin(sstr)) env0;;
print_exp (SetMax(sstr)) env0;;
print_endline ("*** minimo/massimo (ord. lessicografico) di "^(string_of_evT (eval estr env0))^" ***");;
print_exp (SetMin(estr)) env0;;
print_exp (SetMax(estr)) env0;;
print_endline ("*** minimo/massimo (false < true) di "^(string_of_evT (eval lbool env0))^" ***");;
print_exp (SetMin(lbool)) env0;;
print_exp (SetMax(lbool)) env0;;
print_endline ("*** minimo/massimo (false < true) di "^(string_of_evT (eval sbool env0))^" ***");;
print_exp (SetMin(sbool)) env0;;
print_exp (SetMax(sbool)) env0;;
print_endline ("*** minimo/massimo (false < true) di "^(string_of_evT (eval ebool env0))^" ***");;
print_exp (SetMin(ebool)) env0;;
print_exp (SetMax(ebool)) env0;;

(*============== Test per operatori funzionali ==============*)
print_endline "(*============== Test per operatori funzionali ==============*)";;
print_endline "*** Definisco una Fun che ritorna Bool(true) sse l'argomento x != Int 0 ***";;
let notZero = Fun("x", Not(Eq(Den "x", Eint(0))));;

print_endline ("*** Map(inc1, "^(string_of_evT (eval lint env0))^") [incrementa di 1 tutti gli elementi] ***");;
let lint_plus_1 = Map(inc1, lint);;
print_exp (lint_plus_1) env0;;
print_endline "*** Definisco una Fun che concatena all'argomento \"Mod\" ***";;
print_endline ("*** Map(addMod, "^(string_of_evT (eval lstr env0))^") [concatena \"Mod\" ad ogni stringa] ***");;
let addMod = Fun("x", Concat(Den "x", Estring("Mod")));;
print_exp (Map(addMod, lstr)) env0;;

print_endline "*** Unisco i set lint_plus_1 e lint1 ***";;
let union = Merge(lint_plus_1, lint1);;
print_exp union env0;;

print_endline "*** Filtro union secondo la funzione notZero => Elimina Int 0 dal Set union ***";;
print_exp (Filter((notZero, union))) env0;;
print_endline "*** Considero la funzione zero (negata della precedente)\n\te filtro lo stesso set => SetVal([Int 0], \"int\") ***";;
let zero = Fun("x", Eq(Den "x", Eint(0)));;
print_exp (Filter((zero, union))) env0;;