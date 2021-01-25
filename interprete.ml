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

(*Apro il modulo contenente la definizione del linguaggio e l'ambiente*)
open Linguaggio;;

(*============= RTS =============*)
(*type checker (dinamico)*)
let rec typecheck (s : string) (v : evT) : bool =
	(*Definisco una funzione ausiliaria per fare typecheck su liste di denotabili*)
	let rec list_check (t : string) (l : evT list) : bool = 
		match l with
		(*caso base: lista vuota ⇒ true*)
		| [] -> true
		| hd::tl -> if typecheck t hd (*controllo tipo della testa*)
			then list_check t tl (*allora controllo la coda*)
			else false (*il tipo della testa non è t ⇒ false*)
	in match v with (*il typechecker supporta Int, Bool, String e SetVal*)
	| Int(_) -> s = "int"
	| Bool(_) -> s = "bool"
	| String(_) -> s = "string"
	| SetVal(items, set_type) ->
		(*
			Controllo che il tipo del set sia int, bool o string 
			e che tutti i suoi elementi siano di tale tipo. Questo aspetto non
			e' stato evidenziato nella semantica fornita per brevita' ed in quanto
			potrebbero essere supportati ulteriori tipi di set senza dover sostanzialmente
			modificare le regole.
			Se tali condizioni sono soddisfatte allora ho un SetVal 
			valido, quindi restituisco true.
			Altrimenti non lo è, allora false
		*)
		(s = set_type)
		&& ((set_type = "int") || (set_type = "bool") || (set_type = "string"))
		&& (list_check s items)
	(*Se non è nessuno dei precedenti pattern, allora solleva un'eccezione*)
	| _ -> failwith "Error: type not supported by the typechecker"
;;

(*============= Funzioni primitive =============*)
(*richiamate da eval, fanno typechecking ed eseguono l'operazione corrispondente*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with 
		| (Int(n), Int(u)) -> Int(n*u)
		| _,_ -> failwith "Error: cannot apply to the operands"
	else failwith "Type error";;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with
		| Int(n),Int(u) -> Int(n+u)
		| _,_ -> failwith "Error: cannot apply to the operands"
	else failwith "Type error";;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then match (x,y) with 
		| Int(n),Int(u) -> Int(n-u)
		| _,_ -> failwith "Error: cannot apply to the operands"
	else failwith "Type error";;

(*
	Ho modificato eq per accettare anche uguaglianza tra booleani e stringhe
	e poter usare l'operazione Eq(exp, exp) nella definizione di funzioni 
	anche tra bool e stringhe
*)
let eq x y = 
	if	(typecheck "int" x) && (typecheck "int" y)
		|| (typecheck "bool" x) && (typecheck "bool" y)
		|| (typecheck "string" x) && (typecheck "string" y)
	then match x,y with 
		| Int(n), Int(u) -> Bool(n=u)
		| Bool(n), Bool(u) -> Bool(n=u)
		| String(n), String(u) -> Bool(n=u)
		| _,_ -> failwith "Error: cannot apply to the operands"
	else failwith "Type error";;

let minus x = if (typecheck "int" x) 
	then match x with 
		| Int(n) -> Int(-n)
		| _ -> failwith "Error: cannot apply to the operand"
	else failwith "Type error";;

let iszero x = if (typecheck "int" x)
	then match x with 
		| Int(n) -> Bool(n=0)
		| _ -> failwith "Error: cannot apply to the operand"
	else failwith "Type error";;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then match (x,y) with 
		| (Bool(b),Bool(e)) -> Bool(b || e)
		| _,_ -> failwith "Error: cannot apply to the operands"
	else failwith "Type error";;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then match (x,y) with 
		| (Bool(b),Bool(e)) -> Bool(b && e)
		| _,_ -> failwith "Error: cannot apply to the operands"
	else failwith "Type error";;

let non x = if (typecheck "bool" x)
	then match x with
		| Bool(true) -> Bool(false)
		| Bool(false) -> Bool(true)
		| _ -> failwith "Error: cannot apply to the operand"
	else failwith "Type error";;


(*============= Le modifiche apportate =============*)

(*Concatena le String s1 ed s2 con typecheck*)
let concat (s1 : evT) (s2 : evT) : evT = 
	if (typecheck "string" s1) && (typecheck "string" s2)
	then match s1, s2 with 
		| String(x), String(y) -> String(x^y)
		| _,_ -> failwith "Error: either operand is not a String"
	else failwith "Error: type mismatch, either operand is not a string"
;;

(*
	Costruisce il set di tipo t contenente gli elementi nella lista ls,
	dopo aver eseguito (a runtime) il typecheck sugli gli elementi di ls e su t
*)
let setbuild (t : evT) (ls : evT list) : evT =
 	match t with
	| String(s) -> 
		if typecheck s (SetVal(ls, s))
		then SetVal(ls, s)
		else failwith "Error: not a valid Set"
	(*Se t non è una String allora non può essere un tipo (inutile fare il typecheck)*)
	| _ -> failwith "Error: not a valid Set"
;;

(*Rimuove dalla lista ls l'elemento x e restituisce la lista modificata*)
let rec drop_x (ls : evT list) (x : evT) (acc : evT list) = 
	match ls with
	| [] -> acc
	| hd::tl ->
		(*Se la testa è un Int, Bool o String la confronta con x*)
		match hd, x with
		| Int(a), Int(b) -> if a = b 
			then acc@tl
			else drop_x tl x (hd::acc)
		| Bool(a), Bool(b) -> if a = b 
			then acc@tl
			else drop_x tl x (hd::acc)
		| String(a), String(b) -> if a = b 
			then acc@tl
			else drop_x tl x (hd::acc)
		| _,_ -> failwith "Error: type mismatch"
;;

(*============= Operazioni su Set =============*)
(*
	contains ritorna true se elem ∊ items ed ha il tipo del set.
	Solleva un eccezone se i tipi sono diversi
	Altrimenti se elem ∉ items restituisce false.
	Se quello ricevuto non è un SetVal solleva un'eccezione
*)
let contains (set : evT) (elem : evT) : bool = 
	match set with
	| SetVal(items, set_type) -> 
		(*Funzione ausiliaria per trovare l'elemento nella lista*)
		let rec lookup ls x = 
			match ls with
			| [] -> false (*Lista di elementi del set terminata senza trovare elem ⇒ false*)
			| hd::tl -> if hd = x then true else lookup tl x
		in
		(*Controllare che l'elemento cercato abbia lo stesso tipo del set*)
		if typecheck set_type elem
		then lookup items elem
		(*Tipi diversi: eccezione*)
		else failwith "Error: type mismatch"
	| _ -> failwith "Error: not a Set"
;;

(*
	insert tenta di inserire l'elemento newel nel set.
	Se newel ha tipo set_type e se newel ∉ items allora restituisce 
	SetVal(items U {newel}, set_type).
	Altrimenti se newel ∊ items ritorna il set originale e stampa un warning su stdout. 
	Altrimenti se il tipo di newel ≠ set_type solleva un'eccezione
*)
let insert (items : evT list) (set_type : string) (elem : evT) : evT =
	(*Controllo del tipo di newel, se fallisce lancio un'eccezione*)
	if typecheck set_type elem
	(*
		Se lo sono e newel ∉ items lo aggiungo, altrimenti stampo 
		messaggio di warning e restituisco il set originale
	*)
	then 
		if contains (SetVal(items, set_type)) elem
		then (print_endline "Warning: element already in the set"; SetVal(items, set_type))
		else SetVal(elem::items, set_type)
	else failwith "Error: type mismatch"
;;

(*
	remove tenta di rimuovere elem dal set (di cui ricevo gli elementi e il tipo).
	Se il tipo di elem è diverso da quello del set, allora solleva eccezione. 
	Altrimenti, se elem ∊ items allora restituisce il SetVal(items \ {elem}, set_type). 
	Altrimenti ritorna il set originale e stampa un warning su stdout
*)
let remove (items : evT list) (set_type : string) (elem : evT) : evT =
	(*Controllo dinamico dei tipi, se fallisce lancio un'eccezione*)
	if typecheck set_type elem 
	then (*Se elem ∊ items, lo rimuovo*)
		if contains (SetVal(items, set_type)) elem
		(*Uso la funzione drop_x per rimovere elem da items*)
		then SetVal(drop_x items elem [], set_type)
		(*Se elem ∉ items allora stampa warning e ritorna set senza modifiche*)
		else (print_endline "Warning: element not in the set"; SetVal(items, set_type))
	else failwith "Error: type mismatch"
;;

(*
	subset prende in input due set ed i rispettivi tipi (solo per comodità) e ritorna
	true se a ⊆ b ed i tipi combaciano, false altrimenti (solleva eccezione se ta ≠ tb)
	Se a o b non sono SetVal allora solleva un'eccezione (il controllo su b è in contains)
*)
let rec subset (a : evT) (ta : string) (b : evT) (tb : string) : bool =
	(*controllo che i tipi dei due set combacino*)
	if ta = tb
	then match a with
		| SetVal(items, set_type) -> 
			(match items with
			(*Se a è il Set vuoto, allora è sicuramente sottoinsieme di b*)
			| [] -> true
			| hd::tl ->
				(*Se hd è in b allora a può essere sottoinsieme, altrimenti non lo è*)
				if contains b hd
				then subset (SetVal(tl, set_type)) ta b tb
				else false
			)
		| _ -> failwith "Error: not a set"
	(*Se i set sono di tipo diverso allora sollevo un eccezione*)
	else failwith "Error: type mismatch"
;;

(*Funzione ausiliaria che restituisce true sse x < y*)
let lt (x : evT) (y : evT) : bool = 
	match x, y with
	| Int(a), Int(b) -> a < b
	(*Bool(false) < Bool(true), in modo da seguire il comportamento di < in Ocaml*)
	| Bool(a), Bool(b) -> a < b
	(*Per le String uso l'ordine lessicografico, sfruttando <: string->string->bool*)
	| String(s1), String(s2) -> s1 < s2
	| _ -> failwith("Error: total ordering not defined on this type")
;;

(*min della lista di evT passata come parametro, Unbound per il tipo se è vuota*)
let rec min (items : evT list) (m : evT) : evT = 
	match items with
	| [] -> m
	| hd::tl -> 
		(*Se unbound considero la testa come minimo corrente, altrimenti confronto*)
		if (m = UnboundInt) || (m = UnboundBool) || (m = UnboundString) || (lt hd m)
		then min tl hd
		else min tl m
;;

(*max della lista di evT passata come parametro, Unbound per il tipo se è vuota*)
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
	Effettua l'unione dei due set scorrendo gli elementi di uno di essi ed 
	inserendo gli elementi mancanti nell'unione. Un'eccezione viene sollevata se
	uno degli argomenti non fosse un Set, oppure se i set hanno tipi diversi
*)
let rec merge (set1 : evT) (set2 : evT) : evT = 
	match set1, set2 with 
	| SetVal(it1, t1), SetVal(it2, t2) -> 
		if t1 = t2 (*Controllo che i tipi dei set siano uguali*)
		then (*allora scorro la lista it1 per trovare gli elementi da aggiungere all'unione*)
			(match it1 with
			(*Se it1 è la lista vuota allora è sufficiente restituire l'altro set*)
			| [] -> set2
			(*
				altrimenti sfrutto contains ed insert per inserire 
				ricorsivamente gli elementi mancanti in set2
			*)
			| hd::tl -> 
				if contains set2 hd
				then merge (SetVal(tl, t1)) set2
				else merge (SetVal(tl, t1)) (insert it2 t2 hd)
			)
		else failwith "Error: type mismatch. Cannot merge"
	| _,_ -> failwith "Error: either one is not a set"
;;

(*	
	Effettua l'intersezione dei due set intersecando le liste dei loro elementi.
	Uso due funzioni ausiliarie per intersecare le liste e poi restituisco il set
	che ha per elementi tale lista. Se uno degli argomenti non fosse un Set, 
	oppure se i set hanno tipi diversi solleva un'eccezione a runtime
 *)
let rec intersect (set1 : evT) (set2 : evT) : evT =
	match set1, set2 with
	(*Per prima cosa devo ottenere elementi e tipi dei set*)
	| SetVal(it1, type_1), SetVal(it2, type_2) ->
		(*Controllo se i set hanno lo stesso tipo*)
		if type_1 = type_2
		then (*interseca l_1 e l_2 usando una lista ausiliaria acc inizialmente vuota*)
			let rec list_intersect l_1 l_2 acc = 
				(match l_1 with
				| h1::t1 -> if contains (SetVal(it2, type_2)) h1
				(*Se la testa di l_1 è in l_2 allora la aggiungo all'intersezione (acc)*)
					then list_intersect t1 it2 (h1::acc)
					(*Altrimenti interseco la coda di l_1*)
					else list_intersect t1 it2 acc
				(*Ho esaminato l'intera lista, ritorno acc che conterrà l'intersezione*)
				| [] -> acc 
				)
		(*Ritorno il SetVal dato dall'intersezione delle liste di elementi ed il tipo dato*)
		in SetVal(list_intersect it1 it2 [], type_1)
		else failwith "Error: type mismatch. Cannot intersect"
	| _,_ -> failwith "Error: either one is not a set. Cannot intersect"
;;

(*
	Effettua l'operazione di differenza tra insiemi: toglie tutti gli elementi
	della lista to_rm da src e restituisce il SetVal risultante.
	Il controllo sui tipi dei set in input è fatto in eval
*)
let rec setdiff (set1 : evT) (set2 : evT) : evT = 
	(*Per prima cosa devo ottenere elementi e tipi dei set*)
	match set1, set2 with
	| SetVal(it1, type_1), SetVal(it2, type_2) -> 
		(*Controllo se i set hanno lo stesso tipo*)
		if type_1 = type_2
		(*Se i tipi combaciano sottraggo set2 da set1*)
		then (*Scorre la lista di elementi di set2 per rimuoverli da set1*)
			(match it2 with
			(*Lista terminata, ho il set differenza in set1*)
			| [] -> set1
			| hd::tl -> if contains set1 hd
				then setdiff (remove it2 type_1 hd) (SetVal(tl, type_2))
				else setdiff set1 (SetVal(tl, type_2))
			)
		else failwith "Error: type mismatch. Cannot subtract sets"
	| _,_ -> failwith "Error: either one is not a set"
;;

(*============= Funzioni ausiliarie =============*)

(*Converte un esprimibile nell'espressione corrispondente (per Int, Bool e String)*)
let getExp (el : evT) : exp = 
	match el with
	| Int(x) -> Eint(x)
	| Bool(x) -> Ebool(x)
	| String(x) -> Estring(x)
	| _ -> failwith "not a valid type"
;;

(*Converte una lista di esprimibili nella lista di espressioni corrispondente*)
let rec listExp (ls : evT list) (acc : exp list) : (exp list) = 
	match ls with 
	| [] -> acc
	| h::t -> listExp t ((getExp h)::acc)
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
			else failwith "Error: nonboolean guard"
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
					let aVal = eval eArg r in
						(*	rEnv = fDecEnv[g/fClosure] ovvero aggiungo l'associazione del nome della
						 *	funzione ricorsiva all'ambiente al momento della dichiarazione
						 *)
						let rEnv = bind fDecEnv g fClosure in
							(*	aEnv = rEnv[arg/aVal] ovvero lego il param. formale alla valutazione
							 *	del parametro attuale, di fatto realizza il passaggio del parametro
						 	 *)
							let aEnv = bind rEnv arg aVal in
								(*finalmente sono in grado di valutare il corpo di f*)
								eval fBody aEnv
				| _ -> failwith "non functional value"
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
							(*Allora nella valutazione del corpo posso chiamare f stessa*)
			        eval letBody r1
		    | _ -> failwith "non functional def" 
		)
	
	(*============= Le modifiche apportate =============*)
	(*Valutazione di stringhe + typecheck*)
	| Estring s -> if typecheck "string" (String(s)) 
		then String(s) 
		else failwith "Error: not a string"
	(*Valutazione dell'operazione di concatenazione. Il typecheck viene eseguito in concat*)
	| Concat(s1, s2) -> concat (eval s1 r) (eval s2 r)
	(*Valutazione di Set a SetVal. Il typecheck viene eseguito in setbuild*)
	| EmptySet(set_type) -> setbuild (eval set_type r) []
	| Singleton(item, set_type) -> setbuild (eval set_type r) [eval item r]
	| Set(ls, set_type) ->
		(*
			Funzione ausiliaria per inserire senza duplicati la lista di elementi ls
			a partire da un Set vuoto
		 	Il typechecking è effettuato internamente da setbuild. Lancia un'eccezione se
			trova un elemento il cui tipo sia diverso dal tipo del Set
		 *)
		let rec addlist (set : evT) (ls : exp list) : evT = match ls with
			| [] -> set	(*list finita, ritorno il SetVal costruito*)
			| hd::tl -> let el = eval hd r in (*Valuto la testa della lista*)
					if contains set el
					then addlist set tl	(*Se el ∊ set non inserisco*)
					else (match set with
						(*Inserisce el nel set (typecheck in setbuild) e ricorre sulla coda*)
						| SetVal(items, t) -> addlist (setbuild (String(t)) (el::items)) tl
						| _ -> failwith "Error: not a Set"
					)
		in addlist (setbuild (eval set_type r) []) ls
	
	(*============= Operazioni su Set =============*)
	(*	
		IsEmpty restituisce Bool(true) se è il set vuoto, Bool(false) se non lo è 
		e lancia eccezione se l'argomento non è un Set
	 *)
	| IsEmpty(set) -> (match eval set r with
		| SetVal([], _) -> Bool(true)
		| SetVal(ls, _) -> Bool(false)
		| _ -> failwith "Error: not a Set"
		)
	(*
		Restituisce la cardinalità del Set sotto forma di Int
		e lancia eccezione se l'argomento non è un Set
	*)
	| Size(set) -> (match eval set r with
		| SetVal(items, set_type) ->
			if (eval (IsEmpty(set)) r) = Bool(true) 
			then Int(0) (*Caso base: set vuoto ⇒ cardinalità 0*)
			else
				(*ottengo la lista di espressioni dagli evT nella coda e la lego a etail*)
				let etail = (match items with 
				| [] -> []
				| h::t -> listExp t []
				) 
				(*Valuto ricorsivamente l'espressione 1 + Size(Set(etail, type))*)
				in eval (Sum(Eint(1), Size(Set(etail, Estring(set_type))))) r
		| _ -> failwith "Error: not a Set"
		)
	(*
		Contains viene valutata Bool(true) se set contiene elem, Bool(false) altrimenti
		Se non è un Set invece lancia eccezione
	*)
	| Contains(set, elem) -> Bool(contains (eval set r) (eval elem r))
	(*
		Insert viene valuata al SetVal i cui elementi sono l'unione di quelli del
		set ed elem, con eccezione se i tipi non combaciano
	*)
	| Insert(set, elem) -> (match eval set r with
		| SetVal(items, set_type) -> insert items set_type (eval elem r)
		| _ -> failwith "Error: not a set"
		)
	(*Analoga alla Insert, ma rimuove, se presente, elem*)
	| Remove(set, elem) -> (match eval set r with
		| SetVal(items, set_type) -> remove items set_type (eval elem r)
		| _ -> failwith "Error: not a set"
		)
	(*
		Subset(a, b) viene valutata Bool(true) 
		sse (a ⊆ b) ∧ (type_a = type_b), Bool(false) altrimenti
	*)
	| Subset(a, b) -> (match eval a r, eval b r with 
		| SetVal(_,type_a), SetVal(_,type_b) -> 
			Bool(subset (eval a r) type_a (eval b r) type_b)
		| _ -> failwith "Error: either one is not a set"
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
					| _ -> failwith "Error: not a valid set type"
				)
			in min items default
		| _ -> failwith "Error: not a set"
		)
	(*Ritorna il massimo del Set, oppure Unbound[Type] se il set è vuoto*)
	| SetMax(set) -> (match eval set r with
		| SetVal(items, set_type) ->
			(*A seconda del tipo scelgo il valore Unbound da ritornare di default*)
			let default = (match set_type with
					| "int" -> UnboundInt
					| "bool" -> UnboundBool
					| "string" -> UnboundString
					| _ -> failwith "Error: not a valid set type"
				)
			in max items default
		| _ -> failwith("Error: not a set"))
	(*Valuta l'unione di set1 e set2 se il loro tipo coincide*)
	| Merge(set1, set2) -> merge (eval set1 r) (eval set2 r)
	(*Valuta l'intersezione di set1 e set2 se il loro tipo coincide*)
	| Intersect(set1, set2) -> intersect (eval set1 r) (eval set2 r)
	(*
		Effettua la differenza insiemistica {set1} \ {set2}, ovvero
		il set {i : i ∊ set1 ∧ i ∉ set2}
	*)
	| SetDiff(set1, set2) -> setdiff (eval set1 r) (eval set2 r)
	(*
		Restituisce Bool(true) sse tutti gli elementi del Set soddisfano il predicato.
		Il predicato deve essere booleano, ovvero ritornare Bool(_), altrimenti avrò un errore
		di valutazione e la conseguente eccezione a runtime
	*)
	| Forall(pred, set) ->
		(match eval set r with (*Valuto il set per fare typechecking*)
		| SetVal(items, t) ->
			(match items with
			| [] -> Bool(true) (*vacuamente vero se il set non ha elementi*)
			| hd::tl -> 
				(*applico la funzione pred ad hd e controllo il risultato*)
				let res = eval (FunCall(pred, getExp hd)) r in
					(match res with
					(*Se hd soddisfa pred allora valuto se gli elementi della coda lo soddisfano*)
					| Bool(true) ->  eval (Forall(pred, Set(listExp tl [], Estring(t)))) r
					(*Se trovo un elemento che non soddisfa pred ⇒ Bool(false)*)
					| Bool(false) -> Bool(false)
					(*pred non è una funzione booleana, pertanto sollevo un'eccezione*)
					| _ -> failwith "Error: pred ⇏ Bool(_)"
					)
			)
		| _ -> failwith "Error: not a set"
		)
	(*
		Restituisce Bool(true) sse esiste almeno un elemento del Set che 
		soddisfa il pred, altrimenti Bool(false)
	*)
	| Exists(pred, set) -> 
		(match eval set r with (*Valuto il set per fare typechecking*)
		| SetVal(items, t) ->
			(match items with
			| [] -> Bool(false) (*(∄i. i ∊ items ∧ pred(i) ⇒ Bool(true)) allora Bool(false)*)
			| hd::tl -> 
			(*applico la funzione pred ad hd e controllo il risultato*)
				let res = eval (FunCall(pred, getExp hd)) r in
					(match res with
					(*Se trovo un elemento che soddisfa pred ⇒ Bool(true)*)
					| Bool(true) -> Bool(true)
					(*Se hd non soddisfa pred allora valuto se gli elementi della coda lo soddisfano*)
					| Bool(false) ->  eval (Exists(pred, Set(listExp tl [], Estring(t)))) r
					(*pred non è una funzione booleana, pertanto sollevo un'eccezione*)
					| _ -> failwith "Error: pred ⇏ Bool(_)"
					)
			)
		| _ -> failwith "Error: not a set"
		)
	(*Restituisce l'insieme di elementi del set che soddisfano pred*)
	| Filter(pred, set) -> 
		(match eval set r with (*Valuto il set per fare typechecking*)
		| SetVal(items, t) ->
			(match items with
			| [] -> SetVal([], t) (*Se il set è vuoto, allora il set che ritorno è sempre vuoto*)
			| hd::tl -> 
				(*Valuto la testa della lista con la chiamata di pred e argomento hd*)
				let res = eval (FunCall(pred, getExp hd)) r in
					(*Valuto la coda della lista con Filter*)
					let filtered_tail = Filter(pred, Set(listExp tl [], Estring(t))) in
					(match res with
					(*
						Se la testa è stata valutata Bool(true), allora la inserisco nel set che
						risulta dal filtro applicato alla coda
					*)
					| Bool(true) -> eval (Insert(filtered_tail, getExp hd)) r
					(*Altrimenti valuto solo la coda*)
					| Bool(false) -> eval filtered_tail r
					(*pred non booleana: sollevo eccezione*)
					| _ -> failwith "Error: pred ⇏ Bool(_)"
					)
			)
		| _ -> failwith "Error: not a set"
		)
	(*Restituisce il Set in cui ad ogni elemento è stata applicata la funzione funct*)
	| Map(funct, set) ->
		(match eval funct r, eval set r with (*Valuto la funzione e il set*)
			| FunVal(arg, body, decEnv), SetVal(items, t) -> (*Se la funzione non è ricorsiva*)
				(match items with
				| [] -> SetVal([], t) (*Se il set era vuoto, allora restituisco il set vuoto*)
				| hd::tl ->
				(*
					Inserisco nel set prodotto dalla valutazione di Map sulla coda il
					risultato della valutazione di funct con parametro attuale hd
				*)
					let env_plus_hd = bind decEnv arg hd in (*lego argomento al valore hd*)
						let res_hd = eval body env_plus_hd in (*valuto il corpo nel nuovo ambiente*)
							(*
								trovo nuovo tipo del set: l'interprete supporta funzioni della forma
								f: type1 -> type2
							*)
							let new_t = (match res_hd with
								| Int(x) -> "int"
								| Bool(x) -> "bool"
								| String(x) -> "string"
								| _ -> failwith "Error: not a valid set type"
							) in
							(*valuto Map applicato a tl*)
							let tailset = eval (Map(funct, Set(listExp tl [], Estring(t)))) r in
							(match tailset with
							(*Quindi adesso posso costruire il SetVal da restituire*)
							| SetVal(items, set_type) -> SetVal(res_hd::items, new_t)
							| _ -> failwith "Error: not a valid set"
							)
				)
			| _ -> failwith "Error: non-functional value or recursive function. The latter are not supported"
		)
;;


(*Funzione da evT a stringhe, usata per stampare risultato di eval*)
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

(*Funzione che valuta exp in r e stampa il risultato di tale valutazione*)
let print_exp (e : exp) (r : evT env) : unit  = 
	try 
		print_endline (string_of_evT (eval e r));
	with
	| Failure(s) -> print_endline ("Caught "^s)
;;
