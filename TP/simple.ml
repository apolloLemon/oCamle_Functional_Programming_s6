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
let moyenne x y = (x+.y)/.2.;;
(*R -> R -> R*)

let quotienReste = 
	function dvd ->
	function dvs ->
		dvd/dvs :: dvd mod dvs :: []
;;
(* N -> N -> NList*)

(*Local Declarations*)
let puissance4 x = 
	let carre y = y*y in 
		carre x * carre x
;;
(*N->N*)

let toUpper = 
	function c ->
	let cc = Char.code c in
	let ac = Char.code 'a' in
	let zc = Char.code 'z' in
	let delta = ((Char.code 'a')-(Char.code 'A')) in 
	let isLower = function 
		a -> ((a>=ac)&&(a<=zc))
	in if (isLower cc) 
		then Char.chr (cc-delta)
		else c 
;;
(*char -> char*)