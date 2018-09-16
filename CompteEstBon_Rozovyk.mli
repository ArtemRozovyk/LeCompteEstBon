val combinaisons : 'a list -> ('a * 'a * 'a list) list
type expression =
    Valeur of int
  | Mult of expression * expression
  | Moins of expression * expression
  | Plus of expression * expression
  | Div of expression * expression
type nombre = { valeur : int; expr : expression; }
val addition : nombre -> nombre -> nombre list -> nombre list list
val multiplication : nombre -> nombre -> nombre list -> nombre list list
val soustraction : nombre -> nombre -> nombre list -> nombre list list
val division : nombre -> nombre -> nombre list -> nombre list list
val estDans : nombre -> nombre -> (int -> int -> int) -> nombre list -> bool
val to_n : int -> nombre
exception Trouve of nombre
val getRes : nombre list -> int -> nombre
val appl :
  nombre -> nombre -> nombre list -> nombre list -> int -> nombre list list
val combiner : (int * int * int list) list -> int -> nombre list list
val max_n : nombre -> nombre -> int -> nombre
val meilleure_approximation : nombre list -> int -> nombre
val aprox : nombre list list -> int -> nombre list list
val nombre_to_string : nombre -> int * string
val reduire : nombre list list -> int -> bool -> nombre list list
val la_meilleure_combinaison : int list -> int -> int * string
val lire_valeurs : unit -> int list
val menu : string
val partie1 : int list -> (int * string) list
val partie1_to_string : unit -> string
val partie2 : unit -> int * string
val partie2_to_string : unit -> string
val partie3 : int -> int * string
val partie3_to_string : unit -> string
val jeu : unit -> unit
