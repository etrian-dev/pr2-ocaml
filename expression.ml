(*=============== Modulo che definisce le espressioni ===================*)
module Expression = 
struct
(*definisco un identificatore come una stringa*)
type ide = string
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