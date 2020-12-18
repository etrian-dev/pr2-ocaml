(*=========== Modulo che definisce i tipi esprimibili =================*)
module Denotable = 
struct
(*Apro i moduli per espressioni ed ambiente*)
open Expression
open Env

(*Tipi esprimibili*)
type evT = 
	| Int of int
	| Bool of bool
	(*una funzione è una chiusura, la tripla definita sotto *)  
	| FunVal of evFun 
	(*	Una funzione ricorsiva ha bisogno anche del suo nome nella chiusura, altrimenti
	 *	non è possibile valutarla correttamente
	 *)
	| RecFunVal of Expression.ide * evFun
	| Unbound
	(*Ho aggiunto le stringhe ai tipi denotabili*)
	| String of string
	(*	Ho aggiunto i Set ai valori denotabili
	 *	il secondo campo della tupla è il tipo del Set
	 *)
	| SetVal of (evT list) * string
	(*closure: <ide del param. formale, corpo della funzione, ambiente alla dichiarazione>*)
	and evFun = Expression.ide * Expression.exp * evT Env.env
end