(* ('l -> bool) -> 'l list -> 'l list  *)
let rec trouver_tous p l =
	match l with
		t::r -> if (p t) 
			then t::(trouver_tous p r)
			else trouver_tous p r
	| 	[] -> []
;;

(* int -> int list -> int list *)
let ajout n l =
	List.Map ((+)n) l
;;

(* 'l -> 'l list list -> 'l list list *)
let appartient_ssliste x ll =
	trouver_tous (appartient x) ll
;;

let supprime x l = 
	trouver_tous (fun e -> e!=x) l

let supprime_ssliste x ll =
	List.map (supprime x) ll
;;

