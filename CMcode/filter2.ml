let rec longeurListe =
	function
	[] -> 0
| 	prem::rest -> 1+(longeurListe rest)
;; 

let longeur li =
	function rec long n l =
		match l with
			[]->n
		| 	x::r -> long (n+1) r
;;


let rec inlist x = function
	[] -> false
| 	prem::rest -> (prem = x) || inlist x rest
;;

let rec appartient = function x -> function li ->
	match li with
	[]->false
	| y::r -> if x=y
				then true
				else appartient x r
;;

let rec flip = function
	[] -> []
| x::rest -> flip rest @ [x]
;;

(*
let rec fflip li = function 
	[] -> il
| 	x::rest -> flip rest x::il
;; wrong because
With forward recursion,
you always need a helper function
to launch with the result param*)
let renverser li = 
	let rec renv li lr =
		match li with
			[] -> lr
			| x :: r -> renv r (x::lr)

	in renv li []
;;


let rec trouver p li = match li with
	[] -> failwith "not found"
| x::r -> if (p x)
			then x
			else trouver p r
;;