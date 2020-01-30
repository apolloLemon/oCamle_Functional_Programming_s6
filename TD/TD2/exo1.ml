let rec puissance x r =
	match (x,r) with
		(0,0) -> failwith "NO"
	| (n,0) -> 1
	| _ -> x * puissance x (n-1)
;; 