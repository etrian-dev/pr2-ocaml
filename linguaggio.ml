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

(*============= Identificatori (nomi) =============*)
(*definisco un identificatore come una stringa*)
type ide = string;;

(*============= Espressioni =============*)
type exp =
    (*Posso avere costanti intere e booleane*)
    | Eint of int
    | Ebool of bool
    (*Posso avere identificatori*)
    | Den of ide
    (*Varie operazioni aritmetico/logiche*)
    | Prod of exp * exp
    | Sum of exp * exp
    | Diff of exp * exp
    | Eq of exp * exp
    | Minus of exp
    | IsZero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    (*Espressione condizionale: if <guard> then <e1> else <e2>*)
    | Ifthenelse of exp * exp * exp
    (*Let <ide> = <e1> in <e2>*)
    | Let of ide * exp * exp
    (*Dichiarazione di funzione non ricorsiva (un solo parametro)*)
    | Fun of ide * exp
    (*
    	Dichiarazione di funzione ricorsiva (un solo parametro).
    	Rispetto a Fun devo specificare anche il nome della funzione
    *)
    | Letrec of ide * exp * exp
    (*Chiamata di funzione (sia ricorsiva che non ricorsiva)*)
    | FunCall of exp * exp

    (*============= Le modifiche apportate =============*)
	(*	
    Il linguaggio è esteso con i seguenti tipi di espressioni:
    ● Stringhe (Estring)
		● Concatenzione di stringhe (Concat)
		● Set (con annotazione di tipo)
		● varie operazioni su Set
	*)
	
	| Estring of string
	| Concat of exp * exp
	(*
		Ho tre costruttori di Set:
		● Set vuoto (con tipo)
		● Set contenente un singolo elemento (singleton)
		● Set contenente una lista di elementi
		Eventuali duplicati nella lista di espressioni sono scartati al momento
		della valutazione del costruttore, quando avviene anche il typechecking
		sul tipo del Set ed i suoi elementi
	*)
	| EmptySet of exp
	| Singleton of exp * exp
	| Set of (exp list) * exp
	(*============= Operazioni su Set =============*)
	| IsEmpty of exp
	(*Cardinalità del Set passato come argomento*)
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
	(*============= Operatori funzionali su Set =============*)
	| Forall of exp * exp
	| Exists of exp * exp
	| Filter of exp * exp
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
	(*La valutazione di una funzione (non ricorsiva) è una chiusura*)
	| FunVal of evFun
	(*
		Una funzione ricorsiva ha bisogno anche del suo nome nella chiusura,
		altrimenti non è possibile valutarla correttamente
	*)
	| RecFunVal of ide * evFun

	(*============= Le modifiche apportate =============*)
	(*Nuovo denotabile String, risultante dalla valutazione di Estring*)
	| String of string
	(*
		Nuovo denotabile, SetVal, composto da una coppia:
		una di denotabili (elementi) e una stringa (il tipo).
		Il tipo di tutti gli elementi della lista deve corrispondere al tipo del set
		Ciò è verificato a runtime tramite il typechecker.
	*)
	| SetVal of (evT list) * string
	(*
		Costruttori Unbound e Unbound specifici per i tipi usabili nei Set.
		Unbound con tipo usati per la valutazione di SetMin e SetMax.
	*)
	| Unbound
	| UnboundInt
	| UnboundBool
	| UnboundString
	(*closure: <parametro formale, corpo della funzione, ambiente alla dichiarazione>*)
	and evFun = ide * exp * evT env
;;
