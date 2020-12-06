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
		(*Valuta la lista di exp (elementi del Set) a denotabili del linguaggio*)
		let rec evalItems l r = 
			(match l with
		    [] -> []
		    | hd::tl -> (eval hd r)::(evalItems tl r)
			)
		in setbuild (eval set_type r) (evalItems ls r)
	| _ -> failwith("eval failed")
;;

(*============= Le modifiche apportate =============*)
(*Di seguito implemento le operazioni su Set*)
let contains set elem =
	match set with
	| Set(items, set_type) -> 
		(let rec lookup ls x =
			match ls with
			| [] -> false
			| hd::tl ->
				if hd=x then true
				else lookup tl x
			in lookup items elem
		)
	| _ -> failwith("Error: not a Set")
;;

let isempty set = match set with
	| EmptySet(_) -> true
	| Set(items, _) -> (match items with
		| [] -> true
		| hd::tl -> false
	)
	| _ -> let _ = print_endline "Error. Only applicable to sets" in false
;;

(*controlla se il tipo t combacia con il tipo di el (valutati in env)*)
let typematch t el env = match eval t env with
		| String(s) -> typecheck s (eval el env)
		| _ -> failwith("not a type")
;;

(*inserisce, se i tipi combaciano, newel in set (controllando che non sia già presente)*)
let insert set newel env =
	if not (contains set newel) 
	then match set with
	| EmptySet(set_type) -> if typematch set_type newel env
		then Set([newel], set_type)
		else (print_endline "Cannot insert. Reason: type mismatch"; set)
	| Singleton(item, set_type) -> if typematch set_type newel env
		then Set(newel::[item], set_type)
		else (print_endline "Cannot insert. Reason: type mismatch"; set)
	| Set(items, set_type) -> if typematch set_type newel env
		then Set(newel::items, set_type)
		else (print_endline "Cannot insert. Reason: type mismatch"; set)
	| _ -> (print_endline "Cannot insert. Reason: not a Set"; set)
	else (print_endline "Cannot insert. Reason: element already in the set"; set)
;;

let remove set elem =
	match set with
	| Set(items, set_type) -> if contains set elem
		then
			let rec rmEl ls el = match ls with
				| hd::tl -> if hd=el then tl else hd::(rmEl tl el)
				| _ -> failwith("Should not be thrown")
			in Set(rmEl items elem, set_type)
		else failwith("Cannot remove. Reason element not in the Set")
		
	| _ -> failwith("Cannot remove. Reason: not a Set")
;;

let env0 = emptyenv Unbound;;
let eset = EmptySet(Estring("int"));;
let single = Singleton(Estring("Hello"), Estring("string"));;
let intset = Set([Eint(10);Eint(4);Eint(2);Eint(10);Eint(-1);Eint(6)], Estring("int"));;

eval eset env0;;
eval single env0;;
eval intset env0;;