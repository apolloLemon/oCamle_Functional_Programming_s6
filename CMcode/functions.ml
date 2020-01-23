let f x =
	if x == 0
		then 0
		else let a = (x*x)/2 in
			if x>0 
				then a+1
				else a-1 
;; (* int -> int *)
#trace f;;

(*
let carre x = x*x ;; (* int -> int *)

let cube x =
		x * (carre x)
;; (* int -> int *)
*)

let cube x =
		let carre x = x*x in
		x * carre x
;; (* int -> int *)
#trace cube;;

let g x =
	let carre x = x*x in
		let a = (carre x)/2 in
			(a+1)*(a-1)
;;
#trace g;;

let square = 
	function x -> x*x
;;
#trace square;;

let product = 
	function x -> function y -> x*y
;;
#trace product;;

let mymax =
	function x -> function y ->
		if x >= y 
			then x
			else y
;; (* 'a -> 'a -> 'a *)
#trace mymax;; 

(*
(*Tester code*)
cube 2;;

square 2;;

product 2 3 ;;

let multby10 = product 10;;
#trace multby10;;
multby10 2;;


max12 = mymax 12 ;;
#trace max12;;
max12 3;;
*)

let a = 4;;
let b = 3;;
mymax (a) (b);;