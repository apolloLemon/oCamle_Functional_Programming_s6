(* int -> 'l list -> 'l *)
let rec nieme n l =
	match (n,l) with
		(0,x::r) -> x
	| 	(n,x::r) -> nieme (n-1) r
	| 	(n,_) -> failwith "No" 
;;

(*CORRECTION
int -> 'l list -> 'l *)
let rec nieme l n =
	match (l,n) with
		([],_) -> failwith "No"
	| 	(x::_,1) -> x
	| 	(_::r,n) -> nieme r (n-1) 
;;


(* 'a list -> 'a list -> bool *)
let rec prefix p l =
	match (p,l) with
(px::pr,lx::lr) -> if (px=lx) 
			then prefix pr lr
			else false
|(px::pr,_) -> false
|(_,lx::lr) -> true
;;

(*CORRECTION 
'a list -> 'a list -> bool *)
let rec prefix p l =
	match (p,l) with
	[],_ -> true
| 	_,[] -> false
|(px::pr,lx::lr) -> if (px=lx) 
			then prefix pr lr
			else false
;;

(* 'a list -> 'a list -> bool *)
let rec inclu p l =
	match (p,l) with
(px::pr,lx::lr) -> if (px=lx) 
			then prefix pr lr
			else prefix (px::pr) lr
|(px::pr,_) -> false
|(_,lx::lr) -> true
;;

(*CORRECTION
'a list -> 'a list -> bool *)
let rec inclusion l1 l2 =
	if prefix l1 l2 	
		then true
		else match l2 with
			[] -> false
		| _::r2 -> inclusion l1 r2 
;;


(* 'a list -> int*) (* 'a type depends on < PAS VRAI*)
let minimum l =
	match l with
 	[x] -> x
|	x::r -> let min = minimum r in
		if (x<min) 	then x
					else min
| 	_ -> failwith "NO"
;;

(*CORRECTION*)
let minimum l = 
	match l with
	[] -> failwith "No"
| 	[x] -> x 
| 	x::r -> let m = minimum r in
		if (x<m) 	then x
					else m
;;