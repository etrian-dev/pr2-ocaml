(*
#################################################################
# Todo: remove does not work properly yet 											#
#																																#
#																																#
#																																#
#																																#
#																																#
#################################################################
*)

(*============= Identificatori (nomi) =============*)
(*definisco un identificatore come una stringa*)
type ide = string;;

(*============= Espressioni =============*)
(*Ogni espressione legale del linguaggio ha il proprio costruttore*)
type exp =
    (*Posso avere costanti intere e booleane*)
    | Eint of int
    | Ebool of bool
    (*posso avere variabili*)
    | Den of ide 
    (*varie operazioni aritmetico/logiche*)
    | Prod of exp * exp
    | Sum of exp * exp 
    | Diff of exp * exp 
    | Eq of exp * exp 
    | Minus of exp 
    | IsZero of exp 
    | Or of exp * exp 
    | And of exp * exp 
    | Not of exp		
    (*expressione condizionale: if guardia then e1 else e2, con guardia Bool*)						
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    (*dichiarazione di funzione non ricorsiva*) 
    | Fun of ide * exp
    (*dichiarazione di funzione ricorsiva*) 
    | Letrec of ide * exp * exp
    (*chiamata di funzione*)
    | FunCall of exp * exp
    (*============= Le modifiche apportate =============*)
    (*estendo il linguaggio con stringhe, concat. di stringhe e Set (con annotazione di tipo)*)
		| Estring of string
		| Concat of exp * exp
		(*	Ho tre costruttori di Set: set vuoto (tipato), set contenente un singolo elemento
		*	e il costruttore contenente una lista di espressioni
		*)
		| EmptySet of exp
		| Singleton of exp * exp
		| Set of (exp list) * exp
		(*operazioni su Set*)
		| IsEmpty of exp
		| Contains of exp * exp
		| Insert of exp * exp
		| Remove of exp * exp
		| Subset of exp * exp
		| SetMin of exp
		| SetMax of exp
		| Merge of exp * exp
		| Intersect of exp * exp
		| SetDiff of exp * exp
;;

(*============= Ambiente =============*)
(*implementazione dell'ambiente polimorfo come funzione*)
type 't env = ide -> 't;;
(*associo all'ambiente vuoto la funzione che restituisce v*)
let emptyenv (v : 't) = function x -> v;;
(*la funzione ambiente (r) applicata all'identificatore i, ovvero env ▷ i => v*)
let applyenv (r : 't env) (i : ide) = r i;;
(*crea il legame tra l'identificatore i ed il valore v, ovvero env1 = env[v/i]*)
let bind (r : 't env) (i : ide) (v : 't) = 
  function x -> if x = i then v else applyenv r x;;

(*============= Tipi esprimibili =============*)
type evT = 
	| Int of int
	| Bool of bool
	(*una funzione è una chiusura, la tripla definita sotto *)  
	| FunVal of evFun 
	(*	Una funzione ricorsiva ha bisogno anche del suo nome nella chiusura, altrimenti
	 *	non è possibile valutarla correttamente
	 *)
	| RecFunVal of ide * evFun
	| Unbound
	(*============= Le modifiche apportate =============*)
	(*Ho aggiunto le stringhe ai tipi denotabili*)
	| String of string
	(*	Ho aggiunto i Set ai valori denotabili
	 *	il secondo campo della tupla (evT) è il tipo del Set
	 *)
	| SetVal of (evT list) * string
	(*closure: <ide del param. formale, corpo della funzione, ambiente alla dichiarazione>*)
	and evFun = ide * exp * evT env

(*============= RTS =============*)
(*type checking (dinamico)*)
let typecheck (s : string) (v : evT) : bool = match s with
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

(*Removes the elem x from the list ls*)
let rec drop_x ls x acc = match ls with
	| [] -> []
	| hd::tl ->
		match hd, x with
		| Int(a), Int(b) -> if a = b then acc else drop_x tl x (hd::acc)
		| Bool(a), Bool(b) -> if a = b then acc else drop_x tl x (hd::acc)
		| String(a), String(b) -> if a = b then acc else drop_x tl x (hd::acc)
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
	else (print_endline "Type mismatch, cannot insert"; SetVal(items, set_type))
;;

let remove items set_type x =
	(*Se x è nel Set, lo rimuovo*)
	if contains (SetVal(items, set_type)) x
		then SetVal(drop_x items x [], set_type)
		else (print_endline "Element not in the set"; SetVal(items, set_type))
;;

(*Bool(true) se il set a è un sottoinsieme di b*)
let rec subset a b = match a with
	| SetVal(items, set_type) -> 
		(match items with
		| [] -> Bool(true)
		| hd::tl ->
			if contains b hd
			then subset (SetVal(tl, set_type)) b
			else Bool(false)
		)
	| _ -> failwith("not a set")
;;

let lt x y = match x, y with
	| Int(a), Int(b) -> a < b
	| String(s1), String(s2) -> s1 < s2
	| _ -> (print_endline "total ordering not defined"; true)

let rec min items m = match items with
	| [] -> m
	| hd::tl -> 
		if (m = Unbound) || (lt hd m)
		then min tl hd 
		else min tl m
;;

let evTprint el = match el with 
	| Int(x) -> string_of_int x
	| Bool(x) -> string_of_bool x
	| String(x) -> x
	| _ -> ""
;;
let rec max items m = match items with
	| [] -> m
	| hd::tl -> 
		if (m = Unbound) || (lt m hd)
		then (print_endline ("max = "^(evTprint hd)); max tl hd)
		else max tl m;;

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
	| Subset(a, b) -> (match eval a r, eval b r with 
		| SetVal(_,_), SetVal(_,_) -> subset (eval a r) (eval b r)
		| _ -> failwith("either one is not a set"))
	| SetMin(set) -> 
		(match eval set r with
		| SetVal(items, set_type) -> min items Unbound
		| _ -> failwith("not a set"))
	| SetMax(set) -> 
		(match eval set r with
		| SetVal(items, set_type) -> min items Unbound
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
	| _ -> failwith("eval failed")
;;

(*empty env*)
let env0 = emptyenv Unbound;;
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
let lbool = Set([Ebool(true);Ebool(false);Ebool(false);Ebool(true);Ebool(true);Ebool(false);Ebool(false)], Estring("bool"));;
let lstr = Set([Estring("six");Estring("Years");Estring("from");Estring("now");Estring("years");Estring("aaaaaaa")], Estring("string"));;