(*
#################################################################
#																																#
#																																#
#																																#
#																																#
#																																#
#																																#
#################################################################
*)

open Expression
open Env
open Denotable

(*============= RTS =============*)
(*type checking (dinamico)*)
let typecheck (s : string) (v : Denotable.evT) : bool = match s with
	| "int" -> (match v with
			| Int(_) -> true
			| _ -> false)
	| "bool" -> (match v with
			| Bool(_) -> true
			| _ -> false)
	(*Un ulteriore caso del typecheck per il tipo String*)
	| "string" -> (match v with 
		| String(_) -> true
		| _ -> false)
	| _ -> failwith("not a valid type");;

(*============= Funzioni primitive =============*)
(*servono per introdurre più agevolmente il typechecking in eval*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with (Int(n), Int(u)) -> Int(n*u)
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with (Int(n),Int(u)) -> Int(n+u)
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with (Int(n),Int(u)) -> Int(n-u)
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with (Int(n),Int(u)) -> Bool(n=u)
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then match x with  Int(n) -> Int(-n)
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then match x with Int(n) -> Bool(n=0)
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then match (x,y) with (Bool(b),Bool(e)) -> Bool(b || e)
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then match (x,y) with (Bool(b),Bool(e)) -> Bool(b && e)
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then match x with
		| Bool(true) -> Bool(false)
		| Bool(false) -> Bool(true)
	else failwith("Type error");;


(*============= Le modifiche apportate =============*)

(*Ho aggiunto l'operazione di concatenazione di stringhe con typecheck*)
let concat s1 s2 = if (typecheck "string" s1) && (typecheck "string" s2)
	then match (s1, s2) with (String(x), String(y)) -> String(x^y)
	else failwith("Type error");;

(*Costruisce set con tutti i tipi consentiti, usato con tutti i costruttori di set*)
let setbuild t ls = 
	match t with
	| String(s) -> (
		(match s with
		| "int" -> SetVal(ls, "int")
		| "bool" -> SetVal(ls, "bool")
		| "string" -> SetVal(ls, "string")
		| _ -> failwith("Not a valid item type")
		)
	)
	| _ -> failwith("Not a valid Set type");;

(*Rimuove dalla lista ls l'elemento x e restituisce la lista modificata*)
let rec drop_x ls x acc = match ls with
	| [] -> acc
	| hd::tl ->
		match hd, x with
		| Int(a), Int(b) -> if a = b then drop_x tl x acc else drop_x tl x (hd::acc)
		| Bool(a), Bool(b) -> if a = b then drop_x tl x acc else drop_x tl x (hd::acc)
		| String(a), String(b) -> if a = b then drop_x tl x acc else drop_x tl x (hd::acc)
		| _ -> failwith("not a valid type")
;;

(*Di seguito implemento le operazioni su Set*)
let contains set elem =
	match set with
	| SetVal(items, set_type) -> 
		(let rec lookup ls x =
			match ls with
			| [] -> false
			| hd::tl ->
				if (typecheck set_type elem) && (hd = x) then true
				else lookup tl x
			in lookup items elem
		)
	| _ -> failwith("Error: not a Set")
;;

let insert items set_type newel =
	(*prima controllo che il tipo del set e dell'elemento siano uguali*)
	if typecheck set_type newel
	(*Se lo sono inserisco l'elemento, se non era già contenuto nel Set*)
	then if contains (SetVal(items, set_type)) newel
		then 	(print_endline "Element already in the set"; SetVal(items, set_type))
		else SetVal(newel::items, set_type)
	else failwith("Type mismatch")
;;

let remove items set_type x =
	(*Controllo dinamico dei tipi, se fallisce lancio un'eccezione*)
	if typecheck set_type x then
		(*Se x è nel Set, lo rimuovo*)
		if contains (SetVal(items, set_type)) x
			then SetVal(drop_x items x [], set_type)
			else (print_endline "Element not in the set"; SetVal(items, set_type))
	else failwith("Type mismatch")
;;

(*Bool(true) se a è un sottoinsieme di b ed i tipi combaciano, Bool(false) altrimenti*)
let rec subset a ta b tb =
		(*controllo sui tipi*)
		if ta = tb 
		then match a with
			| SetVal(items, set_type) -> 
				(match items with
				(*Se a è il Set vuoto, allora è sottoinsieme di b*)
				| [] -> Bool(true)
				| hd::tl ->
					(*Se hd è in b allora a può essere sottoinsieme: chiamo sulla coda della lista*)
					if contains b hd
					then subset (SetVal(tl, set_type)) ta b tb
					(*Altrimenti non può esserlo*)
					else Bool(false)
				)
			| _ -> failwith("not a set")
		else Bool(false)
;;

let lt x y = match x, y with
	| Int(a), Int(b) -> a < b
	| String(s1), String(s2) -> s1 < s2
	| _ -> failwith("total ordering not defined on this type")

(*Ritorna il minimo della lista di evT passata come parametro, m se vuota*)
let rec min items m = match items with
	| [] -> m
	| hd::tl -> 
		(*	Se era il primo elem della lista aggiorno minimo con hd e chiamo sulla coda
		 *	Altrimenti aggiorno minimo se hd < m e chiamo sulla coda
		 *	Altrimenti chiamo sulla coda senza aggiornare il minimo
		 *)
		if (m = Unbound) || (lt hd m)
		then min tl hd
		else min tl m
;;

(*Ritorna il massimo della lista di evT passata come parametro, m se vuota*)
let rec max items m = match items with
	| [] -> m
	| hd::tl ->
		(*	Se era il primo elem della lista aggiorno massimo con hd e chiamo sulla coda
		 *	Altrimenti aggiorno massimo se m < hd e chiamo sulla coda
		 *	Altrimenti chiamo sulla coda senza aggiornare il massimo
		 *)
		if (m = Unbound) || (lt m hd)
		then max tl hd
		else max tl m
;;

(*	Effettua l'unione dei due set ricorsivamente: se un elemento della lista
 *	l1 non era presente nel set inserisce hd nel set e in ogni caso ricorre 
 *	sulla coda della lista
 *)
let rec merge l1 set = 
	let items, set_type = match set with SetVal(ls, t) -> ls, t in
	match l1 with
	| [] -> set
	| hd::tl -> 
		if contains set hd 
		then merge tl set
		else merge tl (insert items set_type hd)
;;

(*	Effettua l'intersezione dei due set ricorsivamente: se un elemento della lista
 *	l1 non era presente nel set lo rimuove da set e in ogni caso ricorre
 *	sulla coda della lista
 *)
let rec intersect l1 set =
	let items, set_type = match set with SetVal(ls, t) -> ls, t in
	match l1 with
	| [] -> SetVal([], set_type)
	| hd::tl -> 
		if contains set hd 
		then intersect tl set
		else intersect tl (remove items set_type hd)
;;

(*	Effettua l'operazione di sottrazione di insiemi: 
 *	toglie gli elementi di ls presenti in src e restituisce il set risultante
 *)
let rec setdiff src ls = 
	let items, set_type = match src with SetVal(ls, t) -> ls, t in
	match ls with
	| [] -> src
	| hd::tl -> 
		if contains src hd 
		then setdiff (remove items set_type hd) tl
		else setdiff src tl
;;

(*Converte un esprimibile nell'espressione corrispondente*)
let getExp el = match el with
	| Int(x) -> Eint(x)
	| Bool(x) -> Ebool(x)
	| String(x) -> Estring(x)
	| _ -> failwith("not a valid type")
;;

(*Converte una lista di esprimibili nella lista di espressioni corrispondente*)
let rec listexp ls acc = match ls with 
	| [] -> acc
	| h::t -> listexp t [getExp h]@acc
;;

(*============= Valutazione di exp =============*)
(*Prende in input l'espressione e ed un ambiente r (istanziato con il tipo evT)*) 
(*Restituisce un valore esprimibile (tipo evT)*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	| Eint n -> Int n
	| Ebool b -> Bool b
	| IsZero a -> iszero (eval a r)
	(*se ho una variabile applico l'ambiente per ottenere il valore legato*)
	| Den i -> applyenv r i
	| Eq(a, b) -> eq (eval a r) (eval b r)
	| Prod(a, b) -> prod (eval a r) (eval b r)
	| Sum(a, b) -> sum (eval a r) (eval b r)
	| Diff(a, b) -> diff (eval a r) (eval b r)
	| Minus a -> minus (eval a r)
	| And(a, b) -> et (eval a r) (eval b r)
	| Or(a, b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
	(*Valutazione dell'operazione di concatenazione*)
	| Concat(s1, s2) -> concat (eval s1 r) (eval s2 r)
	(*expr condizionale: if g then (eval e1) else (eval e2)*)
	| Ifthenelse(guard, e1, e2) -> let g = eval guard r in 
			if typecheck "bool" g
			    then if g = Bool(true)
				        then (eval e1 r) 
						else (eval e2 r)
			else failwith ("nonboolean guard")
	| Let(i, e1, e2) -> (*let i = e1 in e2*)
	(*r ▷ e1 => v1, poi env[v1/i] ▷ e2 => v2 *)
		eval e2 (bind r i (eval e1 r))
	(* La valutazione della dichiarazione della
	 * funzione i con argomento l'espressione e
	 * è la chiusura riportata
	 *)
	| Fun(i, e) -> FunVal(i, e, r)
	| FunCall(f, eArg) ->
	  (*chiusura di f, distinguo se f è ricorsiva oppure no con pattern matching*)
		let fClosure = (eval f r) in
			(match fClosure with
				| FunVal(arg, fBody, fDecEnv) ->
					(*	Lego la valutazione nell'ambiente corrente del param. attuale 
					 *	all'ide arg (formale) nell'ambiente alla dichiarazione di f 
					 * 	(realizza il passaggio del parametro).
					 *	Con l'ambiente ottenuto valuto il corpo della funzione
					 *)
					eval fBody (bind fDecEnv arg (eval eArg r))
				(*Se invece ho una funzione ricorsiva*)
				| RecFunVal(g, (arg, fBody, fDecEnv)) ->
					(*r ▷ eArg => aVal*)
					let aVal = (eval eArg r) in
						(*	rEnv = fDecEnv[g/fClosure] ovvero aggiungo l'associazione del nome della
						 *	funzione ricorsiva all'ambiente al momento della dichiarazione
						 *)
						let rEnv = (bind fDecEnv g fClosure) in
							(*	aEnv = rEnv[arg/aVal] ovvero lego il param. formale alla valutazione
							 *	del parametro attuale, di fatto realizza il passaggio del parametro
						 	 *)
							let aEnv = (bind rEnv arg aVal) in
								(*finalmente sono in grado di valutare il corpo di f*)
								eval fBody aEnv
				| _ -> failwith("non functional value")
			)
	(*	Valutazione della dichiarazione di funzioni ricorsive
	 *	è un let f = def_f in body, ovvero dichiaro f (ricorsiva) e la utilizzo in body
	 *)
	| Letrec(f, funDef, letBody) ->
		(match funDef with
		    | Fun(i, fBody) ->
					(*	L'ambiente alla dichiarazione deve contenere l'associazione
					 *	tra f e il suo valore (di tipo RecFunVal)
					 *)
			    let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
							(*Allora nella valutzione del corpo posso chiamare f stessa*)
			        eval letBody r1
		    | _ -> failwith("non functional def")
		)
	(*============= Le modifiche apportate =============*)
	(*Valutazione di stringhe*)
	| Estring s -> String s
	(*Valutazione di Set*)
	| EmptySet(set_type) -> setbuild (eval set_type r) []
	| Singleton(item, set_type) -> setbuild (eval set_type r) [eval item r]
	| Set(ls, set_type) ->
		(*	aggiunge una lista di espressioni al set, controllando di non inserire duplicati
		 *	e con typecheck
		 *)
		let rec addlist set ls = match ls with
			(*list finita, ritorno il SetVal costruito*)
			| [] -> set
			| hd::tl ->
				let el = eval hd r in
					if contains set el
					then addlist set tl (*ho trovato un duplicato, non lo inserisco*)
					else (match set with (*non ho duplicati, costruisco il set*)
						| SetVal(items, set_type) ->
							(*typecheck: il tipo del set deve combaciare con l'elemento*)
							if typecheck set_type el
							then addlist (setbuild (String(set_type)) ([el]@items)) tl
							else failwith("type mismatch within elements")
						| _ -> failwith("not a set")
					)
		in addlist (setbuild (eval set_type r) []) ls
	(*Operazioni su Set*)
	| IsEmpty(set) -> (match eval set r with
		| SetVal([], _) -> Bool(true)
		| _ -> Bool(false)
		)
	(*ritorna Bool(true) se set contiene elem, Bool(false) altrimenti*)
	| Contains(set, elem) -> 
			if contains (eval set r) (eval elem r) 
			then Bool(true) 
			else Bool(false)
	| Insert(set, elem) -> 
		(match eval set r with
		| SetVal(items, set_type) -> insert items set_type (eval elem r)
		| _ -> failwith("not a set")
		)
	| Remove(set, elem) -> 
		(match eval set r with
		| SetVal(items, set_type) -> remove items set_type (eval elem r)
		| _ -> failwith("not a set")
		)
	(*Ritorna Bool(true) sse a ⊆ b ∧ ta = tb, Bool(false) altrimenti*)
	| Subset(a, b) -> (match eval a r, eval b r with 
		| SetVal(_,ta), SetVal(_,tb) -> subset (eval a r) ta (eval b r) tb
		| _ -> failwith("either one is not a set")
		)
	(*Ritorna il minimo del Set, oppure Unbound se il set è vuoto*)
	| SetMin(set) -> (match eval set r with
		| SetVal(items, set_type) -> min items Unbound
		| _ -> failwith("not a set"))
	(*Ritorna il massimo del Set, oppure Unbound se il set è vuoto*)
	| SetMax(set) -> (match eval set r with
		| SetVal(items, set_type) -> max items Unbound
		| _ -> failwith("not a set"))
	(*effettua l'unione di set1 e set2, se sono dello stesso tipo*)
	| Merge(set1, set2) -> (match eval set1 r, eval set2 r with
		| SetVal(it1, st1), SetVal(it2, st2) ->
			if st1 = st2
			then merge it1 (eval set2 r)
			else failwith("Type mismatch. Cannot merge")
		| _,_ -> failwith("either one is not a set")
		)
	| Intersect(set1, set2) -> (match eval set1 r, eval set2 r with
		| SetVal(it1, st1), SetVal(it2, st2) ->
			if st1 = st2
			then intersect it1 (eval set2 r)
			else failwith("Type mismatch. Cannot intersect")
		| _,_ -> failwith("either one is not a set")
		)
	(*Effettua la differenza insiemistica set1 - set2, ovvero il set t.c. for all i in set 1 => i not in set2*)
	| SetDiff(set1, set2) -> (match eval set1 r, eval set2 r with
		| SetVal(it1, st1), SetVal(it2, st2) -> 
			if st1 = st2
			(*Se i tipi combaciano sottrae set2 da set1*)
			then setdiff (eval set1 r) it2
			else failwith("Type mismatch. Cannot subtract Sets")
		| _,_ -> failwith("either one is not a set")
		)
	(*Restituisce Bool(true) sse tutti gli elementi del Set soddisfano il predicato*)
	| Forall(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(name, body, environment), SetVal(items, set_type) -> 
				(match items with 
				(*Se il set è vuoto, allora soddisfatto*)
				| [] -> Bool(true)
				(*Altrimenti esamino la lista*)
				| hd::tl -> 
						(*Valuto se la chiamata di funzione ha restituito Bool(true)*)
						if (eval (FunCall(pred, getExp hd)) r) = Bool(true)
						(*Se sì, allora richiamo Forall sul resto della lista*)
						then eval (Forall(pred, Set(listexp tl [], getExp (String(set_type))))) r
						(*Se no, allora pred non è soddisfatto da tutti gli elementi*)
						else Bool(false)
				)
		| _,_ -> failwith("either pred is not a function or not applied to a set")
		)
	(*Restituisce Bool(true) sse esiste almeno un elemento del Set che soddisfa il predicato*)
	| Exists(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(name, body, environment), SetVal(items, set_type) -> 
				(match items with
				(*Se il set è vuoto, allora non esiste, quindi restituisce Bool(false)*)
				| [] -> Bool(false)
				| hd::tl -> 
						if (eval (FunCall(pred, getExp hd)) r) = Bool(true)
						(*Se trovo un elemento che soddisfa pred, restituisco Bool(true)*)
						then Bool(true)
						(*Altrimenti continuo la ricerca sul resto degli elementi*)
						else eval (Exists(pred, Set(listexp tl [], getExp (String(set_type))))) r
				)
		| _,_ -> failwith("either pred is not a function or not applied to a set")
		)
	(*Restituisce l'insieme di elementi che soddisfano pred*)
	| Filter(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(name, body, environment), SetVal(items, set_type) -> 
				(match items with
				(*Se il set è vuoto, restituisce il set vuoto*)
				| [] -> SetVal([], set_type)
				| hd::tl ->
						let filtertail = eval (Filter(pred, Set(listexp tl [], getExp (String(set_type))))) r
						in if (eval (FunCall(pred, getExp hd)) r) = Bool(true)
						(*Se hd soddisfa il predicato, allora lo aggiungo al set e filtro il resto del set*)
						then match filtertail with
							| SetVal(items, set_type) -> insert items set_type hd
							| _ -> failwith("not a set")
						(*Altrimenti continuo a filtrare il resto degli elementi*)
						else filtertail
				)
		| _,_ -> failwith("either pred is not a function or not applied to a set")
		)
	(*Restituisce il Set in cui ad ogni elemento è stata applicata la funzione pred*)
	| Map(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(name, body, environment), SetVal(items, set_type) -> 
				(match items with
				(*Se il set è vuoto, restituisce il set vuoto*)
				| [] -> SetVal([], set_type)
				| hd::tl ->
						(*	per comodità il set formato dal resto della lista
						 *	a cui applico Map lo lego all'identificatore maptail
						 *)
						let maptail = eval (Map(pred, Set(listexp tl [], getExp (String(set_type))))) r
						(*invece pred(hd) = v*)
						in let v = (eval (FunCall(pred, getExp hd)) r)
						(*Inserisco nella valutazione del set della coda il valore v*)
						in match maptail with
							| SetVal(items, set_type) -> insert items set_type v
							| _ -> failwith("not a set")
				)
		| _,_ -> failwith("either pred is not a function or not applied to a set")
		)
;;


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

let a = Fun("x", Sum(Den "x", Eint(1)));;
let p = Fun("x", Eq(Den "x", Eint(0)));;
let map_a = Map(a, lint);;

let merge_int = Merge(lint, lint1);;