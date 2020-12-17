(* =================  TESTS  ================= *)
(*empty env*)
print_endline "dichiaro l'ambiente vuoto";;
let env0 = emptyenv Unbound;;

(*dichiara la funzione function y -> y+1*)
print_endline "dichiaro la funzione che incrementa gli interi di 1";;
let inc1 = Fun("y", Sum(Den "y", Eint 1));;

(*la chiama con arg. 3*)
print_endline "Chiamo inc con argomento 3";;
let e1 = FunCall(inc1, Eint 3);;

(*valuta a partire dall'ambiente vuoto => 4*)
eval e1 env0;;

(*chiama la funzione let x = 2 in function y -> y+x con argomento y=3*)
print_endline "valuto let x=2 in let y=3 in  y + x => 5";;
let e2 = FunCall(Let("x", Eint 2, Fun("y", Sum(Den "y", Den "x"))), Eint 3);;

(*valuta a partire dall'ambiente vuoto => 5*)
eval e2 env0;;
(*valuta a partire dall'ambiente in cui y è legato (in origine) a 10 => 5*)
print_endline "Cambio l'ambiente, ma la valutazione della funzione rimane uguale";;
let env1 = bind env0 "y" (Int(10));;
eval e2 env1;;

(*dichiara le stringhe "Hello, " e "Ocaml"*)
print_endline "Concateno le stringhe hi ed oc";;
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
let sstr = Singleton(Estring("Hello"), Estring("string"));;
(*sets initialized with a list*)
let lint = Set([Eint(10);Eint(10);Eint(-1);Eint(6)], Estring("int"));;
let lint1 = Set([Eint(6);Eint(4);Eint(3);Eint(-1);Eint(6)], Estring("int"));;
let lbool = Set([Ebool(true);Ebool(false);Ebool(false);Ebool(true);Ebool(true);Ebool(false);Ebool(false)], Estring("bool"));;
let lstr = Set([Estring("six");Estring("Years");Estring("from");Estring("now");Estring("years");Estring("aaaaaaa")], Estring("string"));;

print_endline "Definisco le funzioni x -> x + 1 (su interi) e x == 0 (su interi)";;
let add1 = Fun("x", Sum(Den "x", Eint(1)));;
let isZero = Fun("x", Eq(Den "x", Eint(0)));;

print_endline "Applico la funzione a sul Set lint";;
eval (Map(a, lint)) env0;;

print_endline "Unisco il set lint e lint1, poi applico la funzione add1";;
eval (Map((a, Merge(lint, lint1)))) env0;;