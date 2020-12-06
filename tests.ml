
(* =================  TESTS  ================= *)
(* basico: no let *)
let env0 = emptyenv Unbound;;

(*chiama la funzione function y -> y+1 con argomento 3*)
let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;

(*valuta a partire dall'ambiente vuoto => 4*)
eval e1 env0;;

(*chiama la funzione let x = 2 in function y -> y+x con argomento y=3*)
let e2 = FunCall(Let("x", Eint 2, Fun("y", Sum(Den "y", Den "x"))), Eint 3);;

(*valuta a partire dall'ambiente vuoto => 5*)
eval e2 env0;;

(*dichiara le stringhe "Hello, " e "Ocaml"*)
let hi = Estring("Hello, ");;
let oc = Estring("Ocaml");;

