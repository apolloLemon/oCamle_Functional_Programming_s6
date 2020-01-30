(* 'a list -> int -> 'a list *)
let rec npremiers l n = 
	match (l,n) with
	 	_,0 -> [] 
	|	x::r,n -> x::(npremiers r (n-1))
	| 	[],n -> failwith "No" 
;;

(* 'a list list -> 'a list *)
let rec met_a_plat = function
	x::r -> x@(met_a_plat r)
| 	[] -> []
;;

(* 'a list -> 'a list -> 'a list list*)
let rec pair_vers_liste l1 l2 =
	match (l1,l2) with
		x1::r1,x2::r2 -> [x1::x2::[]]::(pair_vers_liste r1 r2)
	| 	[],[] -> [[]]
	| 	_,[] -> failwith "No"
	| 	[],_ -> failwith "No" 
;;

(* 'a list -> 'a list -> 'a list list*)
let rec pair_vers_liste p =
	match p with
		x1::r1,x2::r2 -> (x1,x2)::(pair_vers_liste r1 r2)
	| 	[],[] -> []
	| 	_,[] -> failwith "No"
	| 	[],_ -> failwith "No" 
;;

let liste_vers_pair l =
	let rec getfirsts = function
			(x,y)::r -> x::(getfirsts r)
		| [] -> []
	and getseconds = function
			(x,y)::r -> y::(getseconds r)
		| [] -> []
	in (getfirsts l, getseconds l)
;;

let liste_vers_pair = function 
	[]->([],[])
| (x,y)::r -> let p = liste_vers_pair r in (x::(fst p),y::(snd p)) 
;;
(*
	let rec transfer p = function
			x::r -> (fst p)@[fst x]
				and (snd p)@[snd x]
				and transfer p r
		| 	[] -> p 
	in transfer ([],[])
;;
			[] -> out
		| x::r -> (fst out)@[fst x] 
			and (snd out)@[snd x]
*)

let supprime1 l x =
	match l with
		t::r -> if (t=x) then r else t::(supprime1 r x)
	| 	[] -> []
;;

let rec supprime2 l x =
	match l with
		t::r -> if (t=x) then (supprime2 r x) else t::(supprime2 r x)
	| 	[] -> []
;;

let rec doublon l =
	let rec dansliste m x=
		match m with
			t::r -> (t=x) or dansliste r x
		| 	[] -> false
	in match l with
		t::r -> let o = doublon r in
			if (dansliste o t) 
				then o
				else t::o
	| [] -> [] 
;;	