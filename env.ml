(*=============== Modulo che definisce l'ambiente ===============*)
module Env = 
struct
  (*apro il modulo che definisce le espressioni*)
  open Expression

  (*implementazione dell'ambiente polimorfo come funzione*)
  type 't env = Expression.ide -> 't
  (*associo all'ambiente vuoto la funzione che restituisce v*)
  let emptyenv v = function x -> v
  (*la funzione ambiente (r) applicata all'identificatore i, ovvero env ▷ i => v*)
  let applyenv r i = r i
  (*crea il legame tra l'identificatore i ed il valore v, ovvero env1 = env[v/i]*)
  let bind r i v = function x -> if x = i then v else applyenv r x
end (*end module env*)