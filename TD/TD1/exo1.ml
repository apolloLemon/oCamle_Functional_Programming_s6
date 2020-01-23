function x -> x+3 < 2*x
(*a int -> bool *)

function x -> x && (x<10)
(*b ERREUR de type: X = bool && int*)

function x -> function y -> if x then y else -y
(*c bool -> int -> int *)

f(x,y,z) = if y = "coucou" 	then z(x-4)
							else z(x+3)
(*d (int * string * int -> 'z) -> 'z *)

f x y = if x=1 	then false
				else y(x)
(*e int -> (int -> bool) -> bool *)

f(x,y) = x(y+1) || x(y-1)
(*f ((int -> bool) * int)-> bool *)

function (x,y)->if x(y) then y else false
(*g ((bool -> bool) * bool) -> bool *)

function x -> function y -> x^y
(*h string -> string -> string *)
(* "str"^"str"="strstr" *)

Rond f g = function x -> g(f x)
(*i (('x -> 'f) -> ('f -> 'g) -> 'x -> 'g *)
(* 'x -> 'f -> 'g *)