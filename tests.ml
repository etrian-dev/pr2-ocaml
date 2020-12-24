(*
#################################################################
#																																#
#		PRG2B - Secondo progetto - Nicola Vetrini - matr 600199			#
#		Estensione linguaggio didattico con Set e stringhe, con			#
#		implementazione delle relative operazioni ed estensione			# 
#		del typechecking dinamico.																	#
#																																#
#################################################################
*)

(*Apro il modulo dell'interprete e del linguaggio*)
open Linguaggio;;
open Interprete;;

(* =================  Test generici  ================= *)
print_endline "(* =================  Test generici  ================= *)";;
(*empty env*)
print_endline "*** Dichiaro l'ambiente vuoto ***";;
let env0 = emptyenv Unbound;;

print_endline "*** Dichiaro la funzione inc1 che incrementa l'argomento (intero) di 1 ***";;
let inc1 = Fun("y", Sum(Den "y", Eint 1));;
print_endline "*** Dichiaro la funzione che testa se l'argomento (stringa) è \"Test\" ***";;
let isTest = Fun("s", Eq(Den "s", Estring("Test")));;

(*La chiama con arg. 3 (valuta a partire dall'ambiente vuoto) => Int 4*)
print_endline "*** Chiamo inc1 con argomento Int 3 => Int 4 ***";;
print_exp (FunCall(inc1, Eint 3)) env0;;

(*Modifica l'ambiente con il bind x = 5 e poi chiama la funzione inc1*)
print_endline "*** Estendo env0 con bind x = 5 => env1 ***";
let env1 = bind env0 "x" (Int(5)) in
print_endline "*** Funcall(x -> x+1, x) => Int 6 ***";
print_exp (FunCall(inc1, Den "x")) env1;

(* =================  Test per String  ================= *)
print_endline "(* =================  Test per String  ================= *)";
(*dichiara le stringhe "Hello, " e "Ocaml", poi le concatena*)
let hi = Estring("Test") in
let oc = Estring("Ocaml") in
print_endline ("*** Concatenazione di "^(string_of_evT (eval hi env0))^" ed "^(string_of_evT (eval oc env0))^" ***");
print_exp (Concat(hi, oc)) env0;

(* =================  Test per Set  ================= *)
print_endline "(* =================  Test per Set  ================= *)";
print_endline "*** Dichiaro un set per ogni combinazione di costruttori e tipi ***";
(*empty sets*)
let eint = EmptySet(Estring("int")) in
let ebool = EmptySet(Estring("bool"))in
let estr = EmptySet(Estring("string"))in
(*singletons*)
let sint = Singleton(Eint(0), Estring("int"))in
let sbool = Singleton(Ebool(true), Estring("bool"))in
let sstr = Singleton(oc, Estring("string"))in
(*sets initialized with a list*)
let lint = Set([Eint(10);Eint(10);Eint(-1);Eint(6)], Estring("int"))in
let lint1 = Set([Eint(6);Eint(4);Eint(3);Eint(-1);Eint(18)], Estring("int"))in
let lbool = Set([Ebool(true);Ebool(false);Ebool(false);Ebool(true);Ebool(true);Ebool(false);Ebool(false)], Estring("bool"))in
let lstr = Set([Estring("A");Estring("b");Estring("C");Estring("D");Estring("E");Estring("Ocaml")], Estring("string"))in

(*Valuto tutti i set*)
print_endline "*** Li valuto, ottenendo i denotabili SetVal corrispondenti ***";
print_exp eint env0;
print_exp ebool env0;
print_exp estr env0;
print_exp sint env0;
print_exp sbool env0;
print_exp sstr env0;
print_exp lint env0;
print_exp lbool env0;
print_exp lstr env0;

(*============== Test per IsEmpty ==============*)
print_endline "(*============== Test per IsEmtpy ==============*)";
(*  Testo ciascun set dichiarato con la funzione isEmpty e conto il numero di set vuoti
 *  Questo numero deve risultare 3
 *)
let setlist = [eint; ebool; estr; sint; sbool; sstr; lint; lbool; lstr] in
print_endline "*** Conto numero di set vuoti tra quelli definiti [3] ***";
let rec count_empty ls acc = match ls with
  | [] -> acc
  | hd::tl -> 
    if (eval (IsEmpty(hd)) env0) = Bool(true)
    then count_empty tl acc+1
    else count_empty tl acc
in Printf.printf "numero set vuoti: %d\n" (count_empty setlist 0);

(*============== Test per Size ==============*)
print_endline "(*============== Test per Size ==============*)";
(*Cardinalità di lint => Int 3*)
print_endline ("*** Cardinalità di "^(string_of_evT (eval lint env0))^" [3] ***");
print_exp (Size(lint)) env0;
(*Cardinalità di lint1 => Int 5*)
print_endline ("*** Cardinalità di "^(string_of_evT (eval lint1 env0))^" [5] ***");
(*Cardinalità di sstr => Int 1*)
print_exp (Size(lint1)) env0;
print_endline ("*** Cardinalità di "^(string_of_evT (eval sstr env0))^" [1] ***");
print_exp (Size(sstr)) env0;
(*Cardinalità di ebool => Int 0*)
print_endline ("*** Cardinalità di "^(string_of_evT (eval ebool env0))^" [0] ***");
print_exp (Size(ebool)) env0;

(*============== Test per Contains ==============*)
print_endline "(*============== Test per Contains ==============*)";
print_endline ("*** Int 6 ∊ "^((string_of_evT (eval lint env0)))^"? ***");
print_exp (Contains(lint, Eint(6))) env0;
print_endline ("*** String \"aaa\" ∊ "^(string_of_evT (eval lstr env0))^"? ***");
print_exp (Contains(lstr, Estring("aaa"))) env0;
print_endline ("*** Int 100 ∊ "^(string_of_evT (eval sint env0))^"? ***");
print_exp (Contains(sint, Eint(100))) env0;
print_endline ("*** Bool(true) ∊ "^(string_of_evT (eval ebool env0))^"? ***");
print_exp (Contains(ebool, Ebool(true))) env0;
print_endline ("*** \"test\" ∊ "^(string_of_evT (eval lint1 env0))^"? ***");
print_exp (Contains(lint1, Estring("test"))) env0;

(*============== Test per Insert ==============*)
print_endline "(*============== Test per Insert ==============*)";
print_endline ("*** Inserisco la stringa \"Inserita\" nel set "^((string_of_evT (eval sstr env0)))^" ***");
print_exp (Insert(sstr, Estring("Inserita"))) env0;
print_endline ("*** Inserisco Bool(true) in "^((string_of_evT (eval ebool env0)))^" ***");
print_exp (Insert(ebool, Ebool(true))) env0;
print_endline ("*** Inserisco la stringa \"Ocaml\" nel set "^((string_of_evT (eval sstr env0)))^" [il set rimane invariato + warning] ***");
print_exp (Insert(sstr, oc)) env0;


try
  print_endline ("*** Provo ad inserire la stringa \"42\" in "^((string_of_evT (eval lint env0)))^" [eccezione: errore di tipo] ***");
  print_exp (Insert(lint, Estring("42"))) env0 
with 
| Failure(s) -> print_endline ("Caught "^s);

(*============== Test per Remove ==============*)
print_endline "(*============== Test per Remove ==============*)";
print_endline ("*** Rimuovo Int 3 e Int 18 da "^(string_of_evT (eval lint1 env0))^" ***");
print_exp (Remove((Remove(lint1, Eint(3))), Eint(18))) env0;
print_endline ("*** Rimuovo Int 5 da "^(string_of_evT (eval lint1 env0))^" [set invariato + warning] ***");
print_exp (Remove(lint1, Eint(5))) env0;
print_endline ("*** Rimuovo \"str\" da "^(string_of_evT (eval estr env0))^" ***");
print_exp (Remove(estr, Estring("str"))) env0;

try 
  print_endline ("*** Rimuovo \"str\" da "^(string_of_evT (eval lbool env0))^" [eccezione: errore di tipo] ***");
  print_exp (Remove(lbool, Estring("str"))) env0
with
| Failure(s) -> print_endline ("Caught "^s);

(*============== Test per Subset ==============*)
print_endline "(*============== Test per Subset ==============*)";
print_endline ("*** "^(string_of_evT (eval lint1 env0))^" ⊆ "^(string_of_evT (eval lint env0))^"? ***");
print_exp (Subset(lint1, lint)) env0;
print_endline ("*** "^(string_of_evT (eval (Remove(lint, Eint(10))) env0))^" ⊆ "^(string_of_evT (eval lint1 env0))^"? ***");
print_exp (Subset((Remove(lint, Eint(10))), lint1)) env0;
print_endline ("*** "^(string_of_evT (eval estr env0))^" ⊆ "^(string_of_evT (eval sstr env0))^"? ***");
print_exp (Subset(estr, sstr)) env0;
print_endline ("*** "^(string_of_evT (eval ebool env0))^" ⊆ "^(string_of_evT (eval ebool env0))^"? ***");
print_exp (Subset(ebool, ebool)) env0;
print_endline ("*** "^(string_of_evT (eval lint1 env0))^" ⊆ "^(string_of_evT (eval eint env0)));
print_exp (Subset(lint1, eint)) env0;
print_endline ("*** "^(string_of_evT (eval sint env0))^" ⊆ "^(string_of_evT (eval sbool env0))^"? ***");
print_exp (Subset(sint, sbool)) env0;

(*============== Test per SetMin/SetMax ==============*)
print_endline "(*============== Test per SetMin/SetMax ==============*)";
print_endline ("*** minimo/massimo di "^(string_of_evT (eval lint env0))^" ***");
print_exp (SetMin(lint)) env0;
print_exp (SetMax(lint)) env0;
print_endline ("*** minimo/massimo di "^(string_of_evT (eval sint env0))^" ***");
print_exp (SetMin(sint)) env0;
print_exp (SetMax(sint)) env0;
print_endline ("*** minimo/massimo di "^(string_of_evT (eval eint env0))^" ***");
print_exp (SetMin(eint)) env0;
print_exp (SetMax(eint)) env0;
print_endline ("*** minimo/massimo (ord. lessicografico) di "^(string_of_evT (eval lstr env0))^" ***");
print_exp (SetMin(lstr)) env0;
print_exp (SetMax(lstr)) env0;
print_endline ("*** minimo/massimo (ord. lessicografico) di "^(string_of_evT (eval sstr env0))^" ***");
print_exp (SetMin(sstr)) env0;
print_exp (SetMax(sstr)) env0;
print_endline ("*** minimo/massimo (ord. lessicografico) di "^(string_of_evT (eval estr env0))^" ***");
print_exp (SetMin(estr)) env0;
print_exp (SetMax(estr)) env0;
print_endline ("*** minimo/massimo (false < true) di "^(string_of_evT (eval lbool env0))^" ***");
print_exp (SetMin(lbool)) env0;
print_exp (SetMax(lbool)) env0;
print_endline ("*** minimo/massimo (false < true) di "^(string_of_evT (eval sbool env0))^" ***");
print_exp (SetMin(sbool)) env0;
print_exp (SetMax(sbool)) env0;
print_endline ("*** minimo/massimo (false < true) di "^(string_of_evT (eval ebool env0))^" ***");
print_exp (SetMin(ebool)) env0;
print_exp (SetMax(ebool)) env0;

(*============== Test per Merge ==============*)
print_endline "(*============== Test per Merge ==============*)";
print_endline ("*** Unione di "^(string_of_evT (eval lint1 env0))^" U "^(string_of_evT (eval lint env0))^" ***");
print_exp (Merge(lint1, lint)) env0;
print_endline ("*** Unione di "^(string_of_evT (eval ebool env0))^" U "^(string_of_evT (eval ebool env0))^" ***");
print_exp (Merge(ebool, ebool)) env0;
print_endline ("*** Unione di "^(string_of_evT (eval eint env0))^" U "^(string_of_evT (eval lint env0))^" ***");
print_exp (Merge(eint, lint)) env0;


try
  print_endline ("*** Unione di "^(string_of_evT (eval sstr env0))^" U "^(string_of_evT (eval eint env0))^" ***");
  print_exp (Merge(sstr, eint)) env0 
with
| Failure(s) -> print_endline ("Caught "^s);

try 
  print_endline ("*** Unione di "^(string_of_evT (String("Some text")))^" U "^(string_of_evT (eval eint env0))^" ***");
  print_exp (Merge(Estring("Some text"), eint)) env0 
with
| Failure(s) -> print_endline ("Caught "^s);

(*============== Test per Intersect ==============*)
print_endline "(*============== Test per Intersect ==============*)";
print_endline ("*** Intersezione di "^(string_of_evT (eval lint1 env0))^" ∩ "^(string_of_evT (eval lint env0))^" ***");
print_exp (Intersect(lint1, lint)) env0;
print_endline ("*** Intersezione di "^(string_of_evT (eval lint env0))^" ∩ "^(string_of_evT (eval lint1 env0))^" ***");
print_exp (Intersect(lint, lint1)) env0;
print_endline ("*** Intersezione di "^(string_of_evT (eval eint env0))^" ∩ "^(string_of_evT (eval lint env0))^" ***");
print_exp (Intersect(eint, lint)) env0;
print_endline ("*** Intersezione di "^(string_of_evT (eval lstr env0))^" ∩ "^(string_of_evT (eval lstr env0))^" ***");
print_exp (Intersect(lstr, lstr)) env0;

try 
  print_endline ("*** Intersezione di "^(string_of_evT (eval sstr env0))^" ∩ "^(string_of_evT (eval eint env0))^" ***");
  print_exp (Intersect(sstr, eint)) env0
with
| Failure(s) -> print_endline ("Caught "^s);

try 
  print_endline ("*** Intersezione di "^(string_of_evT (String("Some text")))^" ∩ "^(string_of_evT (eval eint env0))^" ***");
  print_exp (Intersect(Estring("Some text"), eint)) env0
with
| Failure(s) -> print_endline ("Caught "^s);

(*============== Test per SetDiff ==============*)
print_endline "(*============== Test per SetDiff ==============*)";
print_endline ("*** Differenza "^(string_of_evT (eval lint env0))^" ∖ "^(string_of_evT (eval lint env0))^" ***");
print_exp (SetDiff(lint, lint)) env0;
print_endline ("*** Differenza "^(string_of_evT (eval sstr env0))^" ∖ "^(string_of_evT (eval estr env0))^" ***");
print_exp (SetDiff(sstr, estr)) env0;
print_endline ("*** Differenza "^(string_of_evT (eval sstr env0))^" ∖ "^(string_of_evT (eval lstr env0))^" ***");
print_exp (SetDiff(sstr, lstr)) env0;
print_endline ("*** Differenza "^(string_of_evT (eval sint env0))^" ∖ "^(string_of_evT (eval lint env0))^" ***");
print_exp (SetDiff(sint, lint)) env0;

try 
  print_endline ("*** Differenza "^(string_of_evT (eval sint env0))^" ∖ "^(string_of_evT (eval sstr env0))^" ***");
  print_exp (SetDiff(sint, sstr)) env0 
with
| Failure(s) -> print_endline ("Caught "^s);

try 
  print_endline ("*** Differenza "^(string_of_evT (eval sint env0))^" ∖ "^(string_of_evT (Bool(false)))^" ***");
  print_exp (SetDiff(sint, Ebool(false))) env0 
with
| Failure(s) -> print_endline ("Caught "^s);

try 
  print_endline ("*** Differenza "^(string_of_evT (String("Test")))^" ∖ "^(string_of_evT (eval sstr env0))^" ***");
  print_exp (SetDiff(Estring("Test"), sstr)) env0 
with
| Failure(s) -> print_endline ("Caught "^s);

(*============== Test per operatori funzionali ==============*)
print_endline "(*============== Test per operatori funzionali ==============*)";
print_endline ("*** Map(inc1, "^(string_of_evT (eval lint env0))^") [incrementa di 1 tutti gli elementi del Set] ***");
let lint_plus_1 = Map(inc1, lint) in
print_exp (lint_plus_1) env0;

print_endline "*** Definisco una Fun che concatena la stringa \"Mod\" all'argomento ***";
let addMod = Fun("x", Concat(Den "x", Estring("Mod"))) in
print_endline ("*** Map(addMod, "^(string_of_evT (eval lstr env0))^") [concatena \"Mod\" ad ogni stringa] ***");
print_exp (Map(addMod, lstr)) env0;
print_endline ("*** Map(addMod, "^(string_of_evT (eval estr env0))^") [concatena \"Mod\" ad ogni stringa] ***");
print_exp (Map(addMod, estr)) env0;
print_endline "*** Definisco una Fun che ritorna Bool(true) sse l'argomento x != Int 0 ***";
let notZero = Fun("x", Not(Eq(Den "x", Eint(0)))) in
print_endline ("*** Filtro "^(string_of_evT (eval lint_plus_1 env0))^" secondo il predicato notZero [In questo caso elimina Int 0] ***");
print_exp (Filter((notZero, lint_plus_1))) env0;
print_endline "*** Considero la funzione zero (negata della precedente) e filtro lo stesso set [Ottengo il set con il solo 0] ***";
let zero = Fun("x", Eq(Den "x", Eint(0))) in
print_exp (Filter((zero, lint_plus_1))) env0;

print_endline ("*** Forall(zero, "^(string_of_evT (eval eint env0))^") [assumo il predicato vero se il set è vuoto] ***");
print_exp (Forall(zero, eint)) env0;
print_endline ("*** Forall(zero, "^(string_of_evT (eval sint env0))^") ***");
print_exp (Forall(zero, sint)) env0;
print_endline ("*** Forall(zero, "^(string_of_evT (eval lint env0))^") ***");
print_exp (Forall(zero, lint)) env0;

try
  print_endline ("*** Forall(zero, Singleton(Eint(10), Estring(\"string\"))) ⇒ errore di tipo su Set ***");
  print_exp (Forall(zero, Singleton(Eint(10), Estring("string")))) env0
with
| Failure(s) -> print_endline ("Caught "^s);

print_endline "*** Definisco una funzione che ritorna Bool(true) se (x = Int 5) v (x = Int 25), Bool(false) altrimenti***";
let is_5_25 = Fun("x", Ifthenelse(Or(Eq(Den "x", Eint(5)), Eq(Den "x", Prod(Eint(5), Eint(5)))), Ebool(true), Ebool(false))) in
print_endline ("*** Exists(is_5_25, "^(string_of_evT (eval lint env0))^") ***");
print_exp (Exists(is_5_25, lint)) env0;
let lint_union_5 = Merge(lint, Singleton(Eint(5), Estring("int"))) in
print_endline ("*** Exists(is_5_25, "^(string_of_evT (eval lint_union_5 env0))^") ***");
print_exp (Exists(is_5_25, lint_union_5)) env1;
print_endline ("*** Exists(is_5_25, "^(string_of_evT (eval eint env0))^") [falso se vuoto, in quanto (∄i. i ∊ Set ∧ ((i = Int 5) v (i = Int 25)))] ***");
print_exp (Exists(is_5_25, eint)) env0;

try
  print_endline ("*** Forall(addMod, "^(string_of_evT (eval lint env0))^") [funzione che prende String(_) applicata a Int(_) ⇒ errore di tipo] ***");
  print_exp (Forall(addMod, lint)) env0
with
| Failure(s) -> print_endline ("Caught "^s);

try
  
  print_endline ("*** Forall(addMod, "^(string_of_evT (eval sstr env0))^") [funzione non booleana ⇒ errore di valutazione Forall] ***");
  print_exp (Forall(addMod, sstr)) env0
with
| Failure(s) -> print_endline ("Caught "^s);

(*Does not work*)
print_endline "*** Definisco una funzione ricorsiva che calcola il fattoriale di un Int ***";
let fact = 
  Letrec(
      "fact",
      Fun("x", 
        Prod(
          Den "x", 
          Ifthenelse(
            Eq(Den "x", Eint(0)),
            Eint(1),
            FunCall(Den "fact", Diff(Den "x", Eint(1)))
          )
        )
      ),
      FunCall(Den "fact", Eint(2))
  ) in
print_exp fact env0;;