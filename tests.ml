(*
#################################################################
#                                                               #
#   PRG2B - Secondo progetto intermedio - Nicola Vetrini        #
#   Estensione linguaggio didattico con Set e stringhe, con     #
#   implementazione delle relative operazioni ed estensione     # 
#   del typechecking dinamico.                                  #
#                                                               #
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
let inc1 = Fun("param", Sum(Den "param", Eint 1));;
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
try 
  print_endline ("*** \"test\" ∊ "^(string_of_evT (eval lint1 env0))^"? ***");
  print_exp (Contains(lint1, Estring("test"))) env0;
with
| Failure(s) -> print_endline ("Caught"^s);

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
try
  print_endline ("*** "^(string_of_evT (eval sint env0))^" ⊆ "^(string_of_evT (eval sbool env0))^"? ***");
  print_exp (Subset(sint, sbool)) env0;
with
| Failure(s) -> print_endline ("Caught "^s);

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
let lstr1, lstr2 = 
  Set([Estring("a");Estring("b");Estring("c")], Estring("string")),
  Set([Estring("A");Estring("B");Estring("C")], Estring("string")) in
print_endline ("*** Unione di "^(string_of_evT (eval lstr1 env0))^" U "^(string_of_evT (eval lstr2 env0))^" ***");
print_exp (Merge(lstr1, lstr2)) env0;

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

(*============== Definizione di funzioni ==============*)
print_endline "(*============== Definizione di funzioni ==============*)";
print_endline "*** Definisco una Fun che ritorna Bool(true) se il suo argomento è Int 0, Bool(false) altrimenti ***";
print_endline "zero: Int->Bool";
let zero = Fun("x", Eq(Den "x", Eint(0))) in
print_endline "*** Definisco una Fun che ritorna Bool(true) sse l'argomento Int x != Int 0 ***";
print_endline "notZero: Int->Bool";
let notZero = Fun("x", Not(Eq(Den "x", Eint(0)))) in
print_endline "*** Definisco una Fun che ritorna Bool(true) sse l'argomento x = String(\"Ocaml\"), Bool(false) altrimenti ***";
print_endline "findOcaml: String->Bool";
let findOcaml = Fun("x", Eq(Den "x", Estring("Ocaml"))) in
print_endline "*** Definisco una Fun che ritorna String(\"gt\") se Int x > Int 3, altrimenti String(\"le\") ***";
print_endline "gt_le_3: Int->String";
let gt_le_3 = 
  Fun("x", 
    Ifthenelse(
      Eq(
        SetMax(Set([Den "x"; Eint(3)], Estring("int"))),
        Eint(3)
      ),
      Estring("le"),
      Estring("gt")
    )
  ) in

print_endline "*** Definisco una Fun che concatena la stringa \"Mod\" all'argomento ***";
let addMod = Fun("x", Concat(Den "x", Estring("Mod"))) in
print_endline "addMod: String->String";
print_endline "*** Definisco una funzione che ritorna Bool(true) se (x = Int 5) v (x = Int 25), Bool(false) altrimenti***";
print_endline "is_5_25: Int->Bool";
let is_5_25 = 
  Fun("x",
    Ifthenelse(
        Or(
          Eq(Den "x", Eint(5)),
          Eq(Den "x", Eint(25))
        ),
        Ebool(true),
        Ebool(false)
    )
  ) in
print_endline "*** Definisco una funzione ricorsiva che calcola il fattoriale di un Int x (>= 0)***";
print_endline "fact: Int->Int";
let fact = 
  Letrec(
      "fact",
      Fun("x", 
        Ifthenelse(
          Eq(Den "x", Eint(0)),
          Eint(1),
          Prod(
            Den "x",
            FunCall(Den "fact", Diff(Den "x", Eint(1)))
          )
        )
      ),
      (*Il param. formale è arg, deve essere legato a qualche valore*)
      FunCall(Den "fact", Den "arg")
  ) 
in
(*Test*)
print_endline "*** fact(0) ***";
let r0 = bind env0 "arg" (Int(0)) in print_exp fact r0;
print_endline "*** fact(10) ***";
let r1 = bind r0 "arg" (Int(10)) in print_exp fact r1;
print_endline "*** fact(19) ***";
let r2 = bind env0 "arg" (Int(19)) in print_exp fact r2;

print_endline "*** Definisco una funzione ricorsiva, con argomento Int x (>= 0), che calcola Int(2**x) [** è l'elevamento a potenza] ***";
print_endline "exp2: Int->Int";
let exp2 = 
  Letrec(
    "exp2",
    Fun("x",
      Ifthenelse(
        Eq(Den "x", Eint(0)),
        Eint(1),
        Prod(
          Eint(2),
          FunCall(Den "exp2", Diff(Den "x", Eint(1)))
        )
      )
    ),
    (*Il param. formale è arg1, deve essere legato a qualche valore*)
    FunCall(Den "exp2", Den "arg1")
  ) 
in

(*Test*)
print_endline "*** exp2(10) = Int(2)**Int(10) ***";
let r3 = bind env0 "arg1" (Int(10)) in print_exp exp2 r3;
print_endline "*** exp2(10) = Int(2)**Int(0) ***";
let r4 = bind env0 "arg1" (Int(0)) in print_exp exp2 r4;
print_endline "*** exp2(32) = Int(2)**Int(32) ***";
let r5 = bind env0 "arg1" (Int(32)) in print_exp exp2 r5;

(*============== Test per operatori funzionali ==============*)
print_endline "(*============== Test per operatori funzionali ==============*)";

(*Test Forall*)
print_endline ("*** Forall(zero, "^(string_of_evT (eval eint env0))^") [se il set è vuoto assumo Bool(true)] ***");
print_exp (Forall(zero, eint)) env0;
print_endline ("*** Forall(zero, "^(string_of_evT (eval sint env0))^") ***");
print_exp (Forall(zero, sint)) env0;
print_endline ("*** Forall(zero, "^(string_of_evT (eval lint env0))^") ***");
print_exp (Forall(zero, lint)) env0;
print_endline ("*** Forall(is_5_25, "^(string_of_evT (eval lint env0))^") ***");
print_exp (Forall(is_5_25, lint)) env0;

try
  print_endline ("*** Forall(zero, Singleton(Eint(10), Estring(\"string\"))) ⇒ errore di valutazione del Set ***");
  print_exp (Forall(zero, Singleton(Eint(10), Estring("string")))) env0
with
| Failure(s) -> print_endline ("Caught "^s);

try
  print_endline ("*** Forall(zero, "^(string_of_evT (eval lbool env0))^") ⇒ errore di tipo (zero: Int->Bool) ***");
  print_exp (Forall(zero, lbool)) env0;
with
| Failure(s) -> print_endline ("Caught "^s);

(*Test Exists*)
print_endline ("*** Exists(is_5_25, "^(string_of_evT (eval lint env0))^") ***");
print_exp (Exists(is_5_25, lint)) env0;
(*Aggiungo al set precedente Int 5*)
let lint_union_5 = Merge(lint, Singleton(Eint(5), Estring("int"))) in
print_endline ("*** Exists(is_5_25, "^(string_of_evT (eval lint_union_5 env0))^") ***");
print_exp (Exists(is_5_25, lint_union_5)) env0;
print_endline ("*** Exists(is_5_25, "^(string_of_evT (eval eint env0))^") [falso se vuoto, in quanto (∄i. i ∊ Set ∧ ((i = Int 5) v (i = Int 25)))] ***");
print_exp (Exists(is_5_25, eint)) env0;
print_endline ("*** Exists(notZero, "^(string_of_evT (eval lint1 env0))^") ***");
print_exp (Exists(notZero, lint1)) env0;

try
  print_endline ("*** Exists(notZero, "^(string_of_evT (eval lbool env0))^") ⇒ errore di tipo ***");
  print_exp (Exists(notZero, lbool)) env0;
with
| Failure(s) -> print_endline ("Caught "^s);

try
  print_endline ("*** Exists(addMod, "^(string_of_evT (eval sstr env0))^") ⇒ errore: predicato non booleano ***");
  print_exp (Exists(addMod, sstr)) env0;
with
| Failure(s) -> print_endline ("Caught "^s);

(*Test Filter*)
(*definisco un set che contiene alcuni multipli di 5*)
let mult5 = Set([Eint(5);Eint(50);Eint(35);Eint(55);Eint(100);Eint(25);Eint(1555);Eint(15);Eint(10)], Estring("int")) in
print_endline ("*** Filter(is_5_25, "^(string_of_evT (eval mult5 env0))^") ***");
print_exp (Filter(is_5_25, mult5)) env0;
print_endline ("*** Filter(is_5_25, "^(string_of_evT (eval lint env0))^") ***");
print_exp (Filter(is_5_25, lint)) env0;
print_endline ("*** Filter(is_5_25, "^(string_of_evT (eval lint_union_5 env0))^") ***");
print_exp (Filter(is_5_25, lint_union_5)) env0;
print_endline ("*** Filter(findOcaml, "^(string_of_evT (eval lstr env0))^") ***");
print_exp (Filter(findOcaml, lstr)) env0;
try
  print_endline ("*** Filter(findOcaml, "^(string_of_evT (eval sbool env0))^") ⇒ errore di tipo (findOcaml: String->Bool) ***");
  print_exp (Filter(findOcaml, sbool)) env0;
with
| Failure(s) -> print_endline ("Caught "^s);

(*Test Map*)
print_endline ("*** Map(inc1, "^(string_of_evT (eval lint env0))^") [incrementa di 1 tutti gli elementi del Set] ***");
print_exp (Map(inc1, lint)) env0;
print_endline ("*** Map(notZero, "^(string_of_evT (eval lint1 env0))^") ***");
print_exp (Map(notZero, lint1)) env0;
print_endline ("*** Map(is_5_25, "^(string_of_evT (eval mult5 env0))^") ***");
print_exp (Map(is_5_25, mult5)) env0;
let threeStr = Set([Estring("three");Estring("two");Estring("one")], Estring("string")) in
print_endline ("*** Map(addMod, "^(string_of_evT (eval threeStr env0))^") [concatena \"Mod\" ad ogni stringa del set] ***");
print_exp (Map(addMod, threeStr)) env0;
print_endline ("*** Map(addMod, "^(string_of_evT (eval estr env0))^") [concatena \"Mod\" ad ogni stringa] ***");
print_exp (Map(addMod, estr)) env0;
print_endline ("*** Map(findOcaml, "^(string_of_evT (eval threeStr env0))^") ***");
print_exp (Map(findOcaml, threeStr)) env0;
print_endline ("*** Map(findOcaml, "^(string_of_evT (eval lstr env0))^") ***");
print_exp (Map(findOcaml, lstr)) env0;
print_endline ("*** Map(gt_le_3, "^(string_of_evT (eval lint1 env0))^") [String \"gt\" per tutti gli elementi > Int 3, String \"le\" altrimenti] ***");
print_exp (Map(gt_le_3, lint1)) env0;
