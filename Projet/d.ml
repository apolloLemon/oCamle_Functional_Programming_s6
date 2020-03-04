(*type graphe = (int * int list) list;*)

let graphe1 = [	(1, [6;7;8] 	) ; 
				(2, [1;4] 		) ; 
				(3, [2] 		) ; 
				(4, [3;5] 		) ; 
				(5, [1] 		) ; 
				(6, [5;7] 		) ; 
				(7, [] 			) ; 
				(8, [6;7] 		)];;

(* ('a * 'b)list -> 'a list *)
let rec liste_sommet = function
	[] 			-> []
| 	(s,_)::r 	-> s::liste_sommet r
;; 
let liste_sommet g = 
	List.fold_left (fun a (s,_) -> a@[s]) [] g
;; 



(* 'a -> ('a * 'b)list -> 'b list *)
let rec liste_succ = function 
	n -> function
			[] 			-> failwith "404"
		| 	(s,succ)::r -> if s=n 	then succ
									else liste_succ n r
;;
(* ('a * 'b)list -> 'a -> 'b list *)
let rec liste_succ g n = 
	match g with
			[] 			-> failwith "404"
		| 	(s,succ)::r -> if s=n 	then succ
									else liste_succ r n
;;
let rec liste_succ = function 
			[] 			-> failwith "404"
		| 	(s,succ)::r -> function n -> if s=n 	then succ
													else liste_succ r n
;;



(* 'a -> ('b * 'a list) list -> 'b list *)
let rec findPred = function 
	n -> function
		[]		->	[]
| (s,succ)::r 	-> if List.exists (fun p -> if p=n then true else false) succ
						then s::findPred n r
						else findPred n r
;;

(* ('a * 'a list) list -> ('a * 'a list) list *)
let inverser g =
	List.fold_left 
	(fun a (s,succ) -> a@[(s,findPred s g)])
	[] g 
;;


(*
(p,i) est un couple (parcours , information)
C'est 'case memoire' qui va voyager a travers les fold_left


sommet graphe parcours informations
*)
let rec depthProbe s g (p,i)=
	let dPLog s (p,i) = (s::p,i) in (* rajouter un sommet a' la partie parcours du 'couple information' *)
	if (List.mem s i)
		then (p,i) (*On est deja passe' par ici, on retourne par d'ou' on viens avec nos informations*)
		else dPLog s (* rajoute ce sommet dans le parcours retourne' par la suite*)
			(List.fold_left (* On relance sur tout les successeurs *)
				(fun (p,i) s ->
					depthProbe s g (p,i))
				(p,s::i) (*on rejoute l'information comme quoi on est passe' par s*)
				(liste_succ g s) (*Liste des succ pour le fold_left*)
			)
;;



let parcours_prof g =
	let f (x,_) = x in 
		f (List.fold_left 
			(fun (p,i) s ->
				depthProbe s g (p,i))
			([],[]) (* initialisation de (p,i) *)
			(liste_sommet g)
		)
;;
(*
let rec groupProbe s g (gs,i)=
	if(List.mem s i)
		then (gs,i)
		else 
			(List.fold_left (* On relance sur tout les successeurs *)
				(fun (p,i) s ->
					depthProbe s g (p,i))
				(p,s::i) (*on rejoute l'information comme quoi on est passe' par s*)
				(liste_succ g s) (*Liste des succ pour le fold_left*)
			)
;;
(*
*)

let connexites g =
	let ig = inverser g in
		depthProbe 2 ig ([],[])
;;
*)

let rec remove res l1 l2 =
	match l1 with
		| [] -> (List.rev res)
		| h::t -> 	if List.mem h l2 then
						remove res t l2
					else
						remove (h::res) t l2
;;

let connexites graph =
	let suffixe = parcours_prof graph in
	let inverse_suffixe = parcours_prof (inverser graph) in
	let rec rConnexites (suff, invSuff) save res =
		match invSuff with
			| [] -> res
			| h::t -> 	if h=(List.hd suff) then
							rConnexites
								( (remove [] suff (h::t)), (remove [] save (h::t)) ) 
								(remove [] save (h::t))
								[(List.rev (h::t))]@res
						else
							rConnexites
								(suff,t)
								save
								res
	in rConnexites (suffixe,inverse_suffixe) inverse_suffixe []
;;