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

open Linguaggio;;

(*============= RTS =============*)
(*type checking (dinamico)*)
let rec typecheck (s : string) (v : evT) : bool =
	(*Funzione ausiliaria per fare typecheck su liste di valori*)
	let rec list_check (t : string) (l : evT list) : bool = 
		match l with
		| [] -> true (*caso base: vero per la lista vuota*)
		| hd::tl -> 
			if typecheck t hd
			then list_check t tl (*Se è vero per la testa, allora controllo la coda*)
			else false (*Altrimenti esiste un elemento di tipo diverso da t*)
	in match v with
	| Int(_) -> s = "int"
	| Bool(_) -> s = "bool"
	| String(_) -> s = "string"
	| SetVal(items, set_type) -> 
		(*
			Controllo che il tipo del set sia int, bool o string 
			e che tutti gli elementi siano di tale tipo:
			se tali condizioni sono soddisfatte allora ho un SetVal valido,
			altrimenti non lo è
		*)
		(s = set_type)
		&& ((set_type = "int") || (set_type = "bool") || (set_type = "string"))
		&& (list_check s items)
	| _ -> failwith("Error: type not supported by the typechecker")
;;

(*============= Funzioni primitive =============*)
(*servono per introdurre più agevolmente il typechecking in eval*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with 
		| (Int(n), Int(u)) -> Int(n*u)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with
		| (Int(n),Int(u)) -> Int(n+u)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with 
		| (Int(n),Int(u)) -> Int(n-u)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with 
		| (Int(n),Int(u)) -> Bool(n=u)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then match x with 
		| Int(n) -> Int(-n)
		| _ -> failwith("Error: cannot apply to the operand")
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then match x with 
		| Int(n) -> Bool(n=0)
		| _ -> failwith("Error: cannot apply to the operand")
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then match (x,y) with 
		| (Bool(b),Bool(e)) -> Bool(b || e)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then match (x,y) with 
		| (Bool(b),Bool(e)) -> Bool(b && e)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then match x with
		| Bool(true) -> Bool(false)
		| Bool(false) -> Bool(true)
		| _ -> failwith("Error: cannot apply to the operand")
	else failwith("Type error");;


(*============= Le modifiche apportate =============*)

(*Concatena le String s1 ed s2 con typecheck*)
let concat (s1 : evT) (s2 : evT) : evT = 
	if (typecheck "string" s1) && (typecheck "string" s2)
	then match s1, s2 with 
		| String(x), String(y) -> String(x^y)
		| _,_ -> failwith("Error: either operand is not a String")
	else failwith("Type error")
;;

(*	Costruisce set il set di tipo t contenente gli elementi nella lista ls,
 *	dopo aver controllato che tutti gli elementi di ls siano di tipo t e che
 *	t sia uno tra i tipi validi per elementi di Set (int, bool, string)
 *)
let setbuild (t : evT) (ls : evT list) : evT =
 	match t with
	| String(s) -> 
		if typecheck s (SetVal(ls, s))
		then SetVal(ls, s)
		else failwith("Error: not a valid Set")
	(*Se non ho una stringa di tipo errore*)
	| _ -> failwith("Error: not a valid Set type")
;;

(*Rimuove dalla lista ls l'elemento x e restituisce la lista modificata*)
let rec drop_x (ls : evT list) (x : evT) (acc : evT list) = 
	match ls with
	| [] -> acc
	| hd::tl ->
		match hd, x with
		| Int(a), Int(b) -> if a = b 
			then drop_x tl x acc 
			else drop_x tl x (hd::acc)
		| Bool(a), Bool(b) -> if a = b 
			then drop_x tl x acc 
			else drop_x tl x (hd::acc)
		| String(a), String(b) -> if a = b 
			then drop_x tl x acc 
			else drop_x tl x (hd::acc)
		| _ -> failwith("Error: not a valid type")
;;

(*Di seguito implemento le operazioni su Set*)

(*
	contains ritorna true se elem ∊ items e ha lo stesso tipo del set, mentre
	ritorna false se i tipi sono diversi, oppure se elem ∉ items (nel primo caso stampa
	warning). Se quello ricevuto non è un SetVal allora solleva eccezione
*)
let contains (set : evT) (elem : evT) : bool = 
	match set with
	| SetVal(items, set_type) -> 
		let rec lookup ls x =
			match ls with
			| [] -> false (*Lista di elementi del set terminata senza trovare elem => false*)
			| hd::tl ->
				(*Controllare che l'elemento cercato abbia lo stesso tipo del set*)
				if typecheck set_type elem
				then if hd = x then true else lookup tl x
				(*Tipi diversi, restituisce false e stampa warning*)
				else (print_endline "Warning: type mismatch"; false)
			in lookup items elem
	| _ -> failwith("Error: not a Set")
;;

(*
	insert tenta di inserire l'elemento newel nel set, ovvero se newel ha tipo set_type
	e se newel ∉ items allora restituisce SetVal(items U {newel}, set_type). Altrimenti,
	se newel ∊ items ritorna il set originale e stampa un warning. Altrimenti se il tipo
	di newel ≠ set_type solleva eccezione
*)
let insert (items : evT list) (set_type : string) (newel : evT) : evT =
	(*Controllo del tipo di newel, se fallisce lancio un'eccezione*)
	if typecheck set_type newel
	(*
		Se lo sono e newel ∉ items lo aggiungo, altrimenti stampo 
		messaggio di warning e restituisco il set originale
	*)
	then 
		if contains (SetVal(items, set_type)) newel
		then (print_endline "Warning: element already in the set"; SetVal(items, set_type))
		else SetVal(newel::items, set_type)
	else failwith("Error: type mismatch")
;;

(*
	remove riceve in input gli elementi del set (items), il loro tipo 
	e l'elemento da rimuovere (x): se x passa il controllo di tipo e x ∊ items
	allora restituisce il SetVal in cui ho rimosso x, altrimenti ritorna 
	quello ricevuto in input e stampa un warning su standard output. Se invece il tipo
	di x è diverso da quello del set, allora solleva eccezione
*)
let remove (items : evT list) (set_type : string) (x : evT) : evT =
	(*Controllo dinamico dei tipi, se fallisce lancio un'eccezione*)
	if typecheck set_type x 
	then (*Se x ∊ items, lo rimuovo*)
		if contains (SetVal(items, set_type)) x
		then SetVal(drop_x items x [], set_type) (*La funzione drop_x rimove x da items*)
		else (print_endline "Warning: element not in the set"; SetVal(items, set_type))
	else failwith("Error: type mismatch")
;;

(*
	subset prende in input due set ed i rispettivi tipi (solo per comodità) e ritorna
	true se a ⊆ b ed i tipi combaciano, false altrimenti (con warning se ta ≠ tb)
	Se a non è un SetVal allora solleva un'eccezione
*)
let rec subset (a : evT) (ta : string) (b : evT) (tb : string) : bool =
	(*controllo che i tipi dei due set combacino*)
	if ta = tb 
	then match a with
		| SetVal(items, set_type) -> 
			(match items with
			(*Se a è il Set vuoto, allora è sottoinsieme di b*)
			| [] -> true
			| hd::tl ->
				(*Se hd è in b allora a può essere sottoinsieme, altrimenti non lo è*)
				if contains b hd
				then subset (SetVal(tl, set_type)) ta b tb
				else false
			)
		| _ -> failwith("Error: not a set")
	(*Se i set sono di tipi diversi allora sicuramente è false*)
	else (print_endline "warning: type mismatch"; false)
;;

(*Funzione ausiliaria che restituisce true sse x < y *)
let lt (x : evT) (y : evT) : bool = 
	match x, y with
	| Int(a), Int(b) -> a < b
	(*Per i booleani Bool(false) < Bool(true), come da default in Ocaml*)
	| Bool(a), Bool(b) -> a < b
	(*
		Per le stringhe uso l'operatore < : string->string->bool che 
		rispetta l'ordine lessicografico
	*)
	| String(s1), String(s2) -> s1 < s2
	| _ -> failwith("total ordering not defined on this type")
;;

(*Ritorna il min della lista di evT passata come parametro, Unbound per il tipo se è vuota*)
let rec min (items : evT list) (m : evT) : evT = 
	match items with
	| [] -> m
	| hd::tl -> 
		(*Se unbound considero la testa come minimo corrente, altrimenti confronto*)
		if (m = UnboundInt) || (m = UnboundBool) || (m = UnboundString) || (lt hd m)
		then min tl hd
		else min tl m
;;

(*Ritorna il max della lista di evT passata come parametro, Unbound per il tipo se è vuota*)
let rec max (items : evT list) (m : evT) : evT = 
	match items with
	| [] -> m
	| hd::tl ->
		(*Se unbound considero la testa come massimo corrente, altrimenti confronto*)
		if (m = UnboundInt) || (m = UnboundBool) || (m = UnboundString) || (lt m hd)
		then max tl hd
		else max tl m
;;

(*
	Effettua l'unione degli elementi in l1 con quelli in set: se la testa della lista	
	l1 non era presente in set inserisce hd nel set. In ogni caso ricorre 
	sulla coda della lista finchè essa non è vuota e restituisce il SetVal ottenuto.
	Il typechecking è implementato nelle funzioni contains ed insert, perciò
	è garantita la correttezza dei tipi nel set risultante. 
	Il controllo sui tipi dei set in input è fatto in eval
*)
let rec merge (l1 : evT list) (set :evT) : evT = 
	let items, set_type = 
		match set with 
		| SetVal(ls, t) -> ls, t
		| _ -> failwith("Error: not a set")
	in match l1 with
	| [] -> set (*Terminata la lista, ritorno il set unione*)
	| hd::tl -> 
		if contains set hd 
		then merge tl set
		(*Se hd non era nel set, allora lo inserisco tramite insert e poi ricorro*)
		else merge tl (insert items set_type hd)
;;

(*	
	Effettua l'intersezione dei due set a partire dalla lista dei loro elementi.
	Uso due funzioni ausiliarie per intersecare le liste e poi restituisco il set
	che ha per elementi tale lista. Il controllo sui tipi dei set in input è fatto in eval
 *)
let rec intersect (l1 : evT list) (l2 : evT list) (set_type : string) : evT =
	(*funzione ausiliaria che restituisce true sse el ∊ ls, false altrimenti*)
	(*let rec contained ls el = match ls with
		| [] -> false
		| hd::tl -> if hd = el then true else contained tl el
	(*interseca l_1 e l_2 usando una lista ausiliaria acc inizialmente vuota*)
	in*) let rec list_intersect l_1 l_2 acc = match l_1 with
		| h1::t1 ->
			(*Se la testa di l_1 è in l2 allora la aggiungo all'intersezione*)
			if contains (SetVal(l2, set_type)) h1 
			then list_intersect t1 l2 ([h1]@acc)
			else list_intersect t1 l2 acc
		| [] -> acc (*Ho visitato l'intera lista l1, ritorno acc*)
	in SetVal(list_intersect l1 l2 [], set_type)
;;

(*
	Effettua l'operazione di differenza tra insiemi: toglie tutti gli elementi
	di to_rm da src e restituisce il SetVal risultante.
	Il controllo sui tipi dei set in input è fatto in eval
*)
let rec setdiff (src : evT) (to_rm : evT list) : evT = 
	let items, set_type = 
		match src with 
			| SetVal(l, t) -> l, t
			| _ -> failwith("Error: not a set")
	in match to_rm with
	| [] -> src
	| hd::tl ->
	(*
		Se il set sorgente contiene l'elemento hd, allora chiamo la
		funzione di rimozione e poi ricorro sulla coda
	*)
		if contains src hd
		then setdiff (remove items set_type hd) tl
		else setdiff src tl
;;

(*============= Funzioni ausiliarie =============*)
(*Converte un esprimibile nell'espressione corrispondente*)
let getExp (el : evT) : exp = 
	match el with
	| Int(x) -> Eint(x)
	| Bool(x) -> Ebool(x)
	| String(x) -> Estring(x)
	| _ -> failwith("not a valid type")
;;

(*Converte una lista di esprimibili nella lista di espressioni corrispondente*)
let rec listexp (ls : evT list) (acc : exp list) : (exp list) = 
	match ls with 
	| [] -> acc
	| h::t -> listexp t [getExp h]@acc
;;

(*============= Interprete del linguaggio =============*)
(*Prende in input l'espressione e ed un ambiente r (istanziato con il tipo evT)*) 
(*Restituisce un denotabile (evT)*)
let rec eval (e : exp) (r : evT env) : evT = 
	match e with
	| Eint n -> Int n
	| Ebool b -> Bool b
	| IsZero a -> iszero (eval a r)
	(*se ho una variabile applico l'ambiente per ottenere il valore*)
	| Den i -> applyenv r i
	| Eq(a, b) -> eq (eval a r) (eval b r)
	| Prod(a, b) -> prod (eval a r) (eval b r)
	| Sum(a, b) -> sum (eval a r) (eval b r)
	| Diff(a, b) -> diff (eval a r) (eval b r)
	| Minus a -> minus (eval a r)
	| And(a, b) -> et (eval a r) (eval b r)
	| Or(a, b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
	(*expr condizionale: if g then (eval e1) else (eval e2)*)
	| Ifthenelse(guard, e1, e2) -> let g = eval guard r in 
			if typecheck "bool" g
			then if g = Bool(true)
				then eval e1 r 
				else eval e2 r
			else failwith ("Error: nonboolean guard")
	| Let(i, e1, e2) -> (*let i = e1 in e2*)
	(*r ▷ e1 => v1, poi env[v1/i] ▷ e2 => v2 *)
		eval e2 (bind r i (eval e1 r))
	| Fun(i, e) -> FunVal(i, e, r) (*Dichiarazione di funzione => chiusura*)
	| FunCall(f, eArg) -> (*Chiamata di funzione: ottengo la chiusura e *)
	  (*chiusura di f, distinguo se f è ricorsiva oppure no con pattern matching*)
		let fClosure = eval f r in
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
	(*Valutazione dell'operazione di concatenazione, typecheck in concat*)
	| Concat(s1, s2) -> concat (eval s1 r) (eval s2 r)
	(*Valutazione di Set a SetVal. Il typecheck viene eseguito in setbuild*)
	| EmptySet(set_type) -> setbuild (eval set_type r) []
	| Singleton(item, set_type) -> setbuild (eval set_type r) [eval item r]
	| Set(ls, set_type) ->
		(*	Funzione ausiliaria per inserire senza duplicati la lista di elementi ls
		 *	a partire da un Set vuoto
		 *	Il typechecking è effettuato internamente da insert, lancia eccezione se
		 * 	trova un elemento il cui tipo sia diverso dal tipo del Set
		 *)
		let rec addlist (set : evT) (ls : exp list) : evT = match ls with
			| [] -> set	(*list finita, ritorno il SetVal costruito*)
			| hd::tl -> let el = eval hd r in (*Valuto la testa della lista*)
					if contains set el
					then addlist set tl	(*Se el ∊ set non inserisco*)
					else (match set with
						(*Inserisce el nel set (typecheck in setbuild) e ricorre sulla coda*)
						| SetVal(items, t) -> addlist (setbuild (String(t)) ([el]@items)) tl
						| _ -> failwith("Error: not a Set")
					)
		in addlist (setbuild (eval set_type r) []) ls
	(*============= Operazioni su Set =============*)
	(*	Restituisce Bool(true) se è il set vuoto, Bool(false) se non lo è 
	 *	e lancia eccezione se l'argomento non è un Set
	 *)
	| IsEmpty(set) -> (match eval set r with
		| SetVal([], _) -> Bool(true)
		| SetVal(ls, _) -> Bool(false)
		| _ -> failwith("Error: not a Set")
		)
	(*Restituisce la cardinalità del Set sotto forma di Int*)
	| Size(set) -> (match eval set r with
		| SetVal(items, set_type) -> 
			if (eval (IsEmpty(set)) r) = Bool(true) 
			then Int(0)
			else 
				let etail = (match items with 
				| [] -> []
				| h::t -> listexp t []
				) 
				in eval (Sum(Eint(1), Size(Set(etail, Estring(set_type))))) r
		| _ -> failwith("Error: not a Set")
		)
	(*	Contains viene valutata Bool(true) se set contiene elem, Bool(false) altrimenti
	 *	Se non è un Set invece lancia eccezione
	 *)
	| Contains(set, elem) -> Bool(contains (eval set r) (eval elem r))
	(*	Insert viene valuata al SetVal i cui elementi sono l'unione di quelli del
	 *	set ed elem, con eccezione se i tipi non combaciano
	 *)
	| Insert(set, elem) -> (match eval set r with
		| SetVal(items, set_type) -> insert items set_type (eval elem r)
		| _ -> failwith("Error: not a set")
		)
	(*Analoga alla Insert, ma rimuove, se presente, elem*)
	| Remove(set, elem) -> (match eval set r with
		| SetVal(items, set_type) -> remove items set_type (eval elem r)
		| _ -> failwith("Error: not a set")
		)
	(*	Subset(a, b) viene valutata Bool(true) 
	 *	sse (a ⊆ b) ∧ (type_a = type_b), Bool(false) altrimenti
	 *)
	| Subset(a, b) -> (match eval a r, eval b r with 
		| SetVal(_,type_a), SetVal(_,type_b) -> 
			Bool(subset (eval a r) type_a (eval b r) type_b)
		| _ -> failwith("Error: either one is not a set")
		)
	(*Ritorna il minimo del Set, oppure Unbound[Type] se il set è vuoto*)
	| SetMin(set) -> (match eval set r with
		| SetVal(items, set_type) ->
			(*Prima controllo che il set sia ben formato*)
			(*A seconda del tipo scelgo il valore Unbound da ritornare di default*)
			let default = (match set_type with
					| "int" -> UnboundInt
					| "bool" -> UnboundBool
					| "string" -> UnboundString
					| _ -> failwith("Error: not a valid set type")
				)
			in min items default
		| _ -> failwith("Error: not a set"))
	(*Ritorna il massimo del Set, oppure Unbound[Type] se il set è vuoto*)
	| SetMax(set) -> (match eval set r with
		| SetVal(items, set_type) ->
			(*A seconda del tipo scelgo il valore Unbound da ritornare di default*)
			let default = (match set_type with
					| "int" -> UnboundInt
					| "bool" -> UnboundBool
					| "string" -> UnboundString
					| _ -> failwith("Error: not a valid set type")
				)
			in max items default
		| _ -> failwith("Error: not a set"))
	(*Valuta l'unione di set1 e set2 se il loro tipo coincide*)
	| Merge(set1, set2) -> (match eval set1 r, eval set2 r with
		| SetVal(it1, type_1), SetVal(it2, type_2) ->
			if type_1 = type_2
			then merge it1 (eval set2 r)
			else failwith("Error: type mismatch. Cannot merge")
		| _,_ -> failwith("Error: either one is not a set. Cannot merge")
		)
	(*Valuta l'intersezione di set1 e set2 se il loro tipo coincide*)
	| Intersect(set1, set2) -> (match eval set1 r, eval set2 r with
		| SetVal(it1, type_1), SetVal(it2, type_2) ->
			if type_1 = type_2
			then intersect it1 it2 type_1
			else failwith("Error: type mismatch. Cannot intersect")
		| _,_ -> failwith("Error: either one is not a set. Cannot intersect")
		)
	(*	Effettua la differenza insiemistica set1 - set2, ovvero
	 *	il set {i : i ∊ set1 => i ∉ set2}
	 *)
	| SetDiff(set1, set2) -> (match eval set1 r, eval set2 r with
		| SetVal(it1, type_1), SetVal(it2, type_2) -> 
			if type_1 = type_2
			(*Se i tipi combaciano sottraggo set2 da set1*)
			then setdiff (eval set1 r) it2
			else failwith("Error: type mismatch. Cannot subtract")
		| _,_ -> failwith("Error: either one is not a set")
		)
	(*Restituisce Bool(true) sse tutti gli elementi del Set soddisfano il predicato*)
	| Forall(pred, set) -> 
		(*Valuto il set per fare typechecking*)
		(
		try 
			match eval set r with
			| SetVal(items, t) ->
				(match items with
				| [] -> Bool(true)
				| hd::tl -> 
					let res = eval (FunCall(pred, getExp hd)) r in
						(match res with
						| Bool(true) ->  eval (Forall(pred, Set(listexp tl [], getExp (String(t))))) r
						| Bool(false) -> Bool(false)
						| _ -> failwith("Error: pred ⇏ Bool(_)")
						)
				)
			| _ -> failwith("Error: not a set")
		with
		| Failure(s) -> failwith(s)
		)
	(*Restituisce Bool(true) sse esiste almeno un elemento del Set che soddisfa il predicato*)
	| Exists(pred, set) -> 
		(
		try 
			match eval set r with
			| SetVal(items, t) ->
				(match items with
				| [] -> Bool(false)
				| hd::tl -> 
					let res = eval (FunCall(pred, getExp hd)) r in
						(match res with
						| Bool(true) -> Bool(true)
						| Bool(false) ->  eval (Exists(pred, Set(listexp tl [], getExp (String(t))))) r
						| _ -> failwith("Error: pred ⇏ Bool(_)")
						)
				)
			| _ -> failwith("Error: not a set")
		with
		| Failure(s) -> failwith(s)
		)
	(*Restituisce l'insieme di elementi che soddisfano pred*)
	| Filter(pred, set) -> 
		(*Valuto il set per fare typechecking*)
		(
		try 
			match eval set r with
			| SetVal(items, t) ->
				(match items with
				| [] -> SetVal([], t)
				| hd::tl -> 
					let res = eval (FunCall(pred, getExp hd)) r in
						let filtered_tail = Filter(pred, Set(listexp tl [], getExp (String(t)))) in
						(match res with
						| Bool(true) -> 
							eval (Insert(filtered_tail, getExp hd)) r
						| Bool(false) -> 
							eval filtered_tail r
						| _ -> failwith("Error: pred ⇏ Bool(_)")
						)
				)
			| _ -> failwith("Error: not a set")
		with
		| Failure(s) -> failwith(s)
		)
	(*Restituisce il Set in cui ad ogni elemento è stata applicata la funzione pred*)
	| Map(pred, set) ->
		(*Valuto il set per fare typechecking*)
		(
		try 
			match eval set r with
			| SetVal(items, t) ->
				(match items with
				| [] -> SetVal([], t)
				| hd::tl -> 
					eval (Insert(Map(pred, Set(listexp tl [], getExp (String(t)))), FunCall(pred, getExp hd))) r
				)
			| _ -> failwith("Error: not a set")
		with
		| Failure(s) -> failwith(s)
		)
;;


(*Funzione da evT a stringhe, usata per stampare risultato di eval*)
(*Non stampo FunVal perchè equivale a valutarla*)
let rec string_of_evT (obj : evT) : string =
	match obj with
	| Int(x) -> "Int "^(string_of_int x)
	| Bool(x) -> "Bool "^(string_of_bool x)
	| String(x) -> "String \""^x^"\""
	| Unbound -> "Unbound"
	| UnboundInt -> "UnboundInt"
	| UnboundBool -> "UnboundBool"
	| UnboundString -> "UnboundString"
	| SetVal(items, set_type) ->
			let rec to_string ls = match ls with
				| [] -> ""
				| h::[] -> string_of_evT h
				| h::t -> (string_of_evT h)^", "^(to_string t)
			in "SetVal(["^(to_string items)^"], "^set_type^")"
	| _ -> failwith("not supported")
;;

(*	Funzione di supporto per stampare la valutazione 
 *	dell'espressione e valutata nell'ambiente env
 *)
let print_exp (e : exp) (r : evT env) : unit  = 
	print_endline (string_of_evT (eval e r));;