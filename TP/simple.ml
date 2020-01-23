(*Single Variable*)
let ttc ht = ht *. 1.2;;
(* R -> R *)

let bisextile = function 
	a -> ((a mod 4 = 0) && (a mod 100 != 0) || (a mod 400 = 0))	
;;
(* N -> bool *)

let isLower = function 
	c -> (c=Char.lowercase c)
;;
(* char -> bool *)

(*Two Variables*)
