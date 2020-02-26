(*fdiooh*)
(*oqsid*)

let sommerliste l =
	List.fold_left (+) 0 l
;;

let sommetliste l =
	List.fold_left (fun a b -> if a>b then a else b) 0 l
;;

let sommedescarre l =
	sommerliste List.map (fun x -> x*x) l
;;

let sommedescarre_sansmap l =
	List.fold_left (fun a b -> a +(b*b)) 0 l
;;

let sommelongeurliste ll =
	List.fold_left (fun a b -> a + List.length b) 0 ll
;;

let sommelongeurliste ll =
	List.fold_right (fun a b -> List.length a + b) ll 0
;;

let nbroccurances x l =
	List.fold_left (fun n m -> if (x=m) then n+1 else n) 0 l
;;
