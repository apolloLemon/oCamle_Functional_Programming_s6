(* (num -> num -> num) -> List *)
(*
let resolu = function a -> 
	if a=0 	then failwith "a=0"
			else function b -> function c ->
				let delta = (b**2)-(4*a*c) in
					function -> 
						if delta < 0	then failwith "delta<0"
										else let addRes =

						function out -> 
*)

let resolut a b c =
	let delta = (b**2)-(4*a*c) in
		if delta>0	then ((-b-sqrt delta)/(2*a),(-b + sqrt delta)/(2*a))
					else failwith "delta"