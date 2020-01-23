let maximum li = List.fold_right 
	(fun x n-> if x>n then x else n) li (-1);;

let charcount li= List.fold_right
	(fun x n-> String.length x + n ) li (0);;

let appliquer f li= List.fold_right
	(fun x n-> [f x]::n) li ([]);;

let flippy li= List.fold_left
	(fun n x-> x::n) ([]) li;;