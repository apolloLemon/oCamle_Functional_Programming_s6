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
	if y=0 then y=x
	else 




;;