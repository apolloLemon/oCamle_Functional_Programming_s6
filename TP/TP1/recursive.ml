let rec fib =
	function 
		0 -> 0
	| 	1 -> 1
	| 	n -> fib (n-1) + fib (n-2)
;;
(*N->N*)

let rec sommeCarre = function 
		0 -> 0
	| 	n -> (n*n) + sommeCarre (n-1)
;;