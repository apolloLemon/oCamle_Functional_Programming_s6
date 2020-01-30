let rec puissance x n=
	if (n=0) then 1
			else x * (puissance x (n-1))
;;

let rec puissance x n=
	match n with
		0 -> 1.
	| 	_ -> puissance x (n-1)			
;;

let rec puissance = function x -> function 
		0 -> 1
	| 	n -> x * puissance x (n-1)  
;;

(* int -> int -> int*)


let rec puissanceRapide x n =
	if n=0 then 1
	else if n mod 2 = 0 then
			puissanceRapide (x*x) (n/2)
		else puissanceRapide x (n-1)
;;

let puissanceGene x n =
	if x=0 && n <= 0 then Failwith "0"
	else if n>=0
		then puissanceRapide x n
		else 1 / puissanceRapide x (-n)
;;


let rec applyFunc =
	function f ->
	function n ->
	function x ->
		match n with
			0 -> x
		| 	_ -> f (applyFunc f (n-1) x)
;;

(* ('f -> 'f) -> int -> 'x -> 'x *)

let appliquer_n_fois f =
	let rec iter n x =
		if n=0 	then x
				else iter (n-1) (f x)
	in iter
;;

let newPow x n =
	applyFunc (function y -> y*x) n 1
;;

let rec newtonRoot x y eps =
	if (abs (y*y-x) <= eps)
		then y
		else newtonRoot x ((y+(x/y)))/2 eps
;;

let racine x = 
	let eps = 0.0000001 in
	let correct y = 
		(abs (y*y-x) <= eps)
	and suivant y = 
		( (y+(x/y)) / 2 )
	in
	let rec newton y =
		if correct y 	then y
						else newton (suivant y)
	in newton x
;;

(*Appliquer Juska*)

let rec appl_jusque f x b =
	if b(x) then x
			else appl_jusque f (f x) b
;;

let appliquer_jusque f arret =
	let rec iterer x =
		if arret x 	then x
					else iterer (f x)
	in iterer
;;

let racine x =
	let correct y = 
		(abs (y*y-x) <= eps)
	and suivant y = 
		( (y+(x/y)) / 2 )
	in appl_jusque suivant x correct
;;


let racine =
 appliquer_jusque 
 	(fun y -> ( (y+(x/y)) / 2 ))
 	(fun y -> (abs (y*y-x) <= eps))
;;