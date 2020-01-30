type number = I of int | F of float ;;

let somme =
	function x ->
	function 
		y -> x+y
	| 	y -> (float_of_int x) +. y
	| 	y -> x +. (float_of_int y)
	| 	y -> x+.y

;;

let somme n1 n2 = mathch (n1,n2) with
	(I a),(I b) -> I(a+b)
| 	(I a),(F b) -> F((float_of_int a)+.b)
| 	(F a),(I b) -> F(a +.(float_of_int b))
| 	(I a),(I b) -> F(a+.b)
;;

type pile_int = Pvide | Ajout of (int * pile_int);;

let pile_vide = function
	Pvide -> true
| 	_ -> false
;;
let pile_vide p = (p = Pvide);;
(*pile_int -> bool*)

let empiler = 
	function e ->
	function p ->
		Ajout(e,p)
;;
(* int -> pile_int -> pile_int*)

let depiler = function
	Pvide -> failwith "Empty"
| 	Ajout(n,rest) -> rest
;;
(* pile_int -> pile_int*)

let tete = function
	Pvide -> failwith "Empty"
| 	Ajout(n,rest) -> n
;;
(* pile_int -> int*)

Type a pile = Pvide | Ajout of (a * a pile);;

type arbre1 = L of int | N of (string * arbre1 * arbre1) ;;


let rec calculerArbre1 = function
	L(n) -> n
| 	N(s,l,r) ->
		match s with
			"+" -> (calculerArbre1 l) +	(calculerArbre1 l)
		| 	"-" -> (calculerArbre1 l) -	(calculerArbre1 l)
		| 	"*" -> (calculerArbre1 l) *	(calculerArbre1 l)
		| 	"/" -> (calculerArbre1 l) /	(calculerArbre1 l)
;;

let oper = function
	"+" -> (fun x y -> x+y)
| 	"-" -> (fun x y -> x-y)
;;

let rec evalArbre1 = function
	L x -> x
| 	N(op,a1,a2) -> (oper op)(eval a1)(eval a2)
;;

Type a arbre2 = Avide | Noeud of (a * a arbre2 * a arbre2);;

let rec compterNoeud = function
	Avide -> 0
| 	Noeud(_,a1,a2)-> 
	1 + (compterFeuilles a1) + (compterFeuilles a2)
;;
(* a arbre -> int*)

let rec compterFeuilles = function
	Avide -> 0
| 	Noeud(_,Avide,Avide) -> 1 
| 	Noeud(_,a1,a2)-> 
	(compterFeuilles a1) + (compterFeuilles a2)

;;