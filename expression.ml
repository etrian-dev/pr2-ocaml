(*=============== Modulo Expression ===================*)
module type EXPRESSION = 
sig

(*definisco un identificatore come una stringa*)
type ide = string

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
    (*  Estendo il linguaggio con stringhe, concatenazione 
     *  di stringhe e Set con le relative operazioni
     *)
    | Estring of string
    | Concat of exp * exp
    (*	Ho tre costruttori di Set: set vuoto (tipato), 
     *  il set contenente un singolo elemento e il costruttore
     * contenente una lista di espressioni
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
    (*Operatori funzionali su set: prendono un predicato ed un Set*)
    | Forall of exp * exp
    | Exists of exp * exp
    | Filter of exp *exp
    | Map of exp * exp
end (*end module expression*)

(*=============== Modulo Expression ===================*)
module Expression : EXPRESSION = 
struct

(*definisco un identificatore come una stringa*)
type ide = string

(*============= Espressioni =============*)
(*Ogni espressione legale del linguaggio ha il proprio costruttore*)
type exp =
    | Eint of int
    | Ebool of bool
    | Den of ide 
    | Prod of exp * exp
    | Sum of exp * exp 
    | Diff of exp * exp 
    | Eq of exp * exp 
    | Minus of exp 
    | IsZero of exp 
    | Or of exp * exp 
    | And of exp * exp 
    | Not of exp		
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    | Fun of ide * exp
    | Letrec of ide * exp * exp
    | FunCall of exp * exp
    | Estring of string
    | Concat of exp * exp
    | EmptySet of exp
    | Singleton of exp * exp
    | Set of (exp list) * exp
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
    | Forall of exp * exp
    | Exists of exp * exp
    | Filter of exp *exp
    | Map of exp * exp
end (*end module expression*)