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

let rec nb_feuilles = function
	Avide -> 0
| Noeud(_,Avide,Avide) -> 1
| Noeud(_,a1,a2) 

let rec appl_it_arbre2 f base a =
	match a with
	Avide -> base
| 	Noeud(x,a1,a2) -> f x (appl_it_arbre2 f base a1) (appl_it_arbre2 f base a2)
;;

let nb_feuilles a =
	let appl = function x r1 r2 -> if (r1=0 && r2=0) then 1 else (r1+r2)
	and base = 0 in
		appl_it_arbre2 appl base a
;;

let rec mirroir = function
	Avide -> Avide
| Noeud(x,a1,a2) -> Noeud(x, mirroir a2, mirroir a1)
;;

let rec nb_noeud = function
	(_,l) -> 
	let rec sum = function
		[]->0
	| 	x::r -> x + sum r
in 1+sum (List.map nb_feuilles l)
;;

let rec nb_noeud (Noeud(_,li))=
	let plus x y = x+y
in 1+List.fold_left plus 0 (List.map nb_noeud li)
;;

let nb_noeud (Noeud(_,li))=
	let f a b =
		match b with
			_,[] -> a+0
		| _,_::r -> f (a+1) r
	in
	1+List.fold_left f 0
;;


let rec nb_noeud (Noeud(_,li)) =
	1 + List.fold_left (func n a -> n + nb_noeud a) 0 li
;;

let rec list_noeud (Noeud(x,li)) =
	x :: List.fold_left (@) [] (List.map list_noeud li)
;;

let rec list_noeud (Noeud(x,li)) =
	x :: List.fold_left (fun l a -> l@(list_noeud a)) [] (List.map list_noeud li)
;;