(*
let rec factorial = 
	function
	0 -> 1
| 	x -> x * (factorial (x-1))
;;
#trace factorial;;

factorial 3;;


let min_paire = function p ->
	if (fst p < snd p) then fst p
						else snd p
;;
#trace min_paire;;

min_paire (1,2);;



let somme = function (x,y) -> x+y ;;
#trace somme;;
somme (2,3);;


*)

(*These following functions
use exclusively filtering*)
let vide = function
	[] -> true
| 	 _ -> false
;;
#trace vide;;
vide [];;
vide [1;2];;

let singleton = function
  	[_]-> true
|	_ -> false
;;
#trace singleton;;
singleton [1];;
singleton [];;
singleton [3;4];;
(*
*)
let tete = function
	x :: efwkbu -> x
| 	[] -> failwith "Liste Vide"
;;
#trace tete;;
tete [1;2];;
tete [1];;
tete [];;

let qeueue = function
	x :: efwkbu -> efwkbu
| 	[] -> failwith "Liste Vide"
;;
#trace qeueue;;
qeueue [1;2];;
qeueue [1];;
qeueue [];;

let deuxieme = function
	x :: efwkbu :: _-> efwkbu
| 	_ -> failwith "Liste trop petite"
;;
#trace deuxieme;;
deuxieme [1;2];;
deuxieme [1];;
deuxieme [];;

(*
*)