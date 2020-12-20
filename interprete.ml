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
		| Size of exp
		| Contains of exp * exp
		| Insert of exp * exp
		| Remove of exp * exp
		| Subset of exp * exp
		| SetMin of exp
		| SetMax of exp
		| Merge of exp * exp
		| Intersect of exp * exp
		| SetDiff of exp * exp
		(*Operatori funzionali su set*)
		| Forall of exp * exp
		| Exists of exp * exp
		| Filter of exp *exp
		| Map of exp * exp
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
	(*============= Le modifiche apportate =============*)
	(*Ho aggiunto le stringhe ai tipi denotabili*)
	| String of string
	(*	Ho aggiunto i Set ai valori denotabili
	 *	il secondo campo della tupla (evT) è il tipo del Set
	 *)
	| SetVal of (evT list) * string
	(*Valore Unbound + Unbound specifici per i tipi, usati nella valutazione di SetMin e SetMax*)
	| Unbound
	| UnboundInt
	| UnboundBool
	| UnboundString
	(*closure: <ide del param. formale, corpo della funzione, ambiente alla dichiarazione>*)
	and evFun = ide * exp * evT env

(*============= RTS =============*)
(*type checking (dinamico)*)
let typecheck (s : string) (v : evT) : bool = 
	match s with
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

(*	Funzione ausiliaria per controllare che il tipo di ogni elemento di ls
 *	Coincida con quello del Set
 *)
let rec list_check (t : string) (l : evT list) : bool = 
 	match l with
	| [] -> true (*caso base: vero per la lista vuota*)
	| hd::tl -> 
		if typecheck t hd
		then list_check t tl (*Se è vero per la testa, allora controllo la coda*)
		else false (*Altrimenti esiste un elemento di tipo diverso da t*)
;;

(*	Funzione ausiliaria per controllare che il SetVal sia valido, ovvero che abbia tipo
 *	int, bool o string e tutti i suoi elementi abbiano tale tipo
 *)
let set_check (set : evT) : bool = 
	match set with
	| SetVal(items, set_type) ->
		if	((set_type = "int") || (set_type = "bool") || (set_type = "string"))
				&& (list_check set_type items) then true
			else false
	| _ -> failwith("Error: not a SetVal")
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

(*Concatena s1 ed s2 con typecheck*)
let concat s1 s2 = if (typecheck "string" s1) && (typecheck "string" s2)
	then match (s1, s2) with 
		| (String(x), String(y)) -> String(x^y)
		| _,_ -> failwith("Error: cannot apply to the operands")
	else failwith("Type error");;

(*	Costruisce set il set di tipo t contenente gli elementi nella lista ls,
 *	dopo aver controllato che tutti gli elementi di ls siano di tipo t e che
 *	t sia uno tra i tipi validi per elementi di Set (int, bool, string)
 *)
let setbuild (t : evT) (ls : evT list) : evT =
 	match t with
	| String(s) -> 
		if set_check (SetVal(ls, s))
		then SetVal(ls, s) 
		else failwith("Error: not a valid Set")
	(*Se non ho una stringa di tipo errore*)
	| _ -> failwith("Error: not a valid Set type")
;;

(*Rimuove dalla lista ls l'elemento x e restituisce la lista modificata*)
let rec drop_x ls x acc = match ls with
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
(*contains ritorna true se set contiene elem*)
let contains (set : evT) (elem : evT) : bool = 
	match set with
	| SetVal(items, set_type) -> 
		(let rec lookup ls x =
			match ls with
			| [] -> false
			| hd::tl ->
				(*typecheck per assicurarmi che l'elemento cercato abbia lo stesso tipo del set*)
				if typecheck set_type elem
				then if hd = x then true else lookup tl x
				(*Tipi diversi, restituisco false + warning*)
				else (print_endline "Warning: type mismatch";false)
			in lookup items elem
		)
	| _ -> failwith("Error: not a Set")
;;

let insert (items : evT list) (set_type : string) (newel : evT) : evT =
	(*Controllo dinamico dei tipi, se fallisce lancio un'eccezione*)
	if typecheck set_type newel
	(*	Se lo sono e newel ∉ items lo aggiungo, altrimenti stampo 
	 *	messaggio di warning e restituisco il set originale
	 *)
	then 
		if contains (SetVal(items, set_type)) newel
		then (print_endline "Warning: element already in the set"; SetVal(items, set_type))
		else SetVal(newel::items, set_type)
	else failwith("Error: type mismatch")
;;

let remove (items : evT list) (set_type : string) (x : evT) : evT =
	(*Controllo dinamico dei tipi, se fallisce lancio un'eccezione*)
	if typecheck set_type x 
	then (*Se x ∊ items, lo rimuovo*)
		if contains (SetVal(items, set_type)) x
		then SetVal(drop_x items x [], set_type) (*La funzione drop_x rimove x da items*)
		else (print_endline "Warning: element not in the set"; SetVal(items, set_type))
	else failwith("Error: type mismatch")
;;

(*true se a ⊆ b ed i tipi combaciano, false altrimenti*)
let rec subset (a : evT) (ta : string) (b : evT) (tb : string) =
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

(*Funzione ausiliaria per confrontare elementi*)
let lt x y = match x, y with
	| Int(a), Int(b) -> a < b
	(*Per i booleani Bool(false) < Bool(true), come da default di Ocaml*)
	| Bool(a), Bool(b) -> a < b
	| String(s1), String(s2) -> s1 < s2
	| _ -> failwith("total ordering not defined on this type")
;;

(*Ritorna il minimo della lista di evT passata come parametro, m se vuota*)
let rec min items m = match items with
	| [] -> m
	| hd::tl -> 
		(*	Se era il primo elem della lista aggiorno minimo con hd e chiamo sulla coda
		 *	Altrimenti aggiorno minimo se hd < m e chiamo sulla coda
		 *	Altrimenti chiamo sulla coda senza aggiornare il minimo
		 *)
		if (m = UnboundInt) || (m = UnboundBool) || (m = UnboundString) || (lt hd m)
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
		if (m = UnboundInt) || (m = UnboundBool) || (m = UnboundString) || (lt m hd)
		then max tl hd
		else max tl m
;;

(*	Effettua l'unione dei due set ricorsivamente: se la testa della lista
 *	l1 non era presente in set inserisce hd nel set. In ogni caso ricorre 
 *	sulla coda della lista finchè non è vuota
 *)
let rec merge l1 set = 
	let items, set_type = 
		(match set with 
		| SetVal(ls, t) -> ls, t
		| _ -> failwith("not a set")
		)
	in match l1 with
	| [] -> set
	| hd::tl -> 
		if contains set hd 
		then merge tl set
		(*Se hd non era nel set, allora lo inserisco tramite insert e poi ricorro*)
		else merge tl (insert items set_type hd)
;;

(*	Effettua l'intersezione dei due set a partire dalla lista dei loro elementi.
 *	Uso due funzioni ausiliarie per intersecare le liste e poi restituisco il set
 *	che ha per elementi tale lista
 *)
let rec intersect l1 l2 set_type =
	(*restituisce true sse el ∊ ls, false altrimenti*)
	let rec contained ls el = match ls with
		| [] -> false
		| hd::tl -> if hd = el then true else contained tl el
	(*interseca l_1 e l_2 usando una lista ausiliaria acc inizialmente vuota*)
	in let rec list_intersect l_1 l_2 acc = match l_1 with
		| h1::t1 ->
			(*Se la testa di l_1 è in l2 allora la aggiungo all'intersezione*)
			if contained l2 h1
			then list_intersect t1 l2 ([h1]@acc)
			else list_intersect t1 l2 acc
		| [] -> acc (*Ho visitato l'intera lista l1, ritorno acc*)
	in SetVal(list_intersect l1 l2 [], set_type)
;;

(*	Effettua l'operazione di sottrazione di insiemi: 
 *	toglie gli elementi di to_rm presenti in src e restituisce il set risultante
 *)
let rec setdiff src to_rm = 
	let items, set_type = 
		match src with 
			| SetVal(l, t) -> l, t
			| _ -> failwith("Error: not a set")
	in match to_rm with
	| [] -> src
	| hd::tl ->
	(*	Se il set sorgente contiene l'elemento hd, allora chiamo la
	 *	funzione di rimozione e poi ricorro sulla coda
	 *)
		if contains src hd
		then setdiff (remove items set_type hd) tl
		else setdiff src tl
;;

(*============= Funzioni ausiliarie =============*)
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
	(*Valutazione dell'operazione di concatenazione*)
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
	| Forall(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(_,_,decEnv), SetVal(items, set_type) -> 
				(match items with 
				(*Se il set è vuoto, allora è vero per definizione*)
				| [] -> Bool(true)
				(*Altrimenti esamino la lista di elementi*)
				| hd::tl -> 
						(*	Chiamo la funzione pred, valutandola nell'ambiente di dichiarazione,
						 *	con argomento hd. Se il risultato è Bool(true), allora proseguo,
						 *	altrimenti posso dire che esiste un elemento che non soddisfa il
						 *	predicato. Una particolarità di procedere in questo modo è la necessità
						 *	di ottenere l'espressione da passare a FunCall a partire dall'esprimibile 
						 *	hd e lo realizzo con la chiamata di getExp. Analogamente, per effettuare 
						 *	la chiamata ricorsiva devo ottenere la lista di exp da una evT list ed a 	
						 *	questo scopo ho definito la funzione listexp
						 *)
						let res = eval (FunCall(pred, getExp hd)) decEnv in
						if typecheck "bool" res && res = Bool(true)
						then eval (Forall(pred, Set(listexp tl [], getExp (String(set_type))))) r
						else 	
							if res = Bool(false) 
							then Bool(false) 
							else failwith("Error: pred ⇏ Bool(_)")
				)
		| _,_ -> failwith("Error: either pred ⇏ FunVal(_,_,_) or set ⇏ SetVal(_,_)")
		)
	(*Restituisce Bool(true) sse esiste almeno un elemento del Set che soddisfa il predicato*)
	| Exists(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(_, _, decEnv), SetVal(items, set_type) -> 
				(match items with
				(*Se il set è vuoto, allora non esiste, quindi restituisce Bool(false)*)
				| [] -> Bool(false)
				| hd::tl ->
						let res = eval (FunCall(pred, getExp hd)) decEnv in
						(*Codice analogo alla ForAll, ma con diverse condizioni*)
						if typecheck "bool" res && res = Bool(true)
						(*Se trovo un elemento che soddisfa pred, restituisco Bool(true)*)
						then Bool(true)
						(*Altrimenti continuo la ricerca sul resto degli elementi*)
						else 
							if res = Bool(false) 
							then eval (Exists(pred, Set(listexp tl [], getExp (String(set_type))))) r 
							else failwith("Error: pred ⇏ Bool(_)")
				)
		| _,_ -> failwith("Error: either pred ⇏ FunVal(_,_,_) or set ⇏ SetVal(_,_)")
		)
	(*Restituisce l'insieme di elementi che soddisfano pred*)
	| Filter(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(_, _, decEnv), SetVal(items, set_type) -> 
				(match items with
				(*Se il set è vuoto, restituisce il set vuoto*)
				| [] -> SetVal([], set_type)
				| hd::tl ->
						let filtertail = eval (Filter(pred, Set(listexp tl [], getExp (String(set_type))))) r in 
						if (eval (FunCall(pred, getExp hd)) decEnv) = Bool(true)
						(*Se hd soddisfa il predicato, allora lo aggiungo al set e filtro il resto del set*)
						then match filtertail with
							| SetVal(items, set_type) -> insert items set_type hd
							| _ -> failwith("Error: set ⇏ SetVal(_,_)")
						(*Altrimenti continuo a filtrare il resto degli elementi*)
						else filtertail
				)
		| _,_ -> failwith("Error: either pred ⇏ FunVal(_,_,_) or set ⇏ SetVal(_,_)")
		)
	(*Restituisce il Set in cui ad ogni elemento è stata applicata la funzione pred*)
	| Map(pred, set) -> (match eval pred r, eval set r with 
		| FunVal(_, _, decEnv), SetVal(items, set_type) -> 
				(match items with
				(*Se il set è vuoto, restituisce il set vuoto*)
				| [] -> SetVal([], set_type)
				| hd::tl ->
						(*	per comodità il set formato dal resto della lista
						 *	a cui applico Map lo lego all'identificatore maptail
						 *)
						let maptail = 
							eval (Map(pred, Set(listexp tl [], getExp (String(set_type))))) r
						(*invece pred(hd) = v*)
						in let v = (eval (FunCall(pred, getExp hd)) decEnv)
						(*Inserisco nella valutazione del set della coda il valore v*)
						in match maptail with
							| SetVal(items, set_type) -> insert items set_type v
							| _ -> failwith("Error: set  SetVal(_,_)")
				)
		| _,_ -> failwith("Error: either pred ⇏ FunVal(_,_,_) or set ⇏ SetVal(_,_)")
		)
;;

(*Funzione da evT a stringhe, usata per stampare risultato di eval*)
(*Non stampo FunVal perchè equivale a valutarla*)
let rec string_of_evT obj = match obj with
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
let print_exp e env = print_endline (string_of_evT (eval e env));;