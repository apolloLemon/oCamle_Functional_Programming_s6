(* Code for Ocaml project 

*)

(* ###################### *)
(* ##### EXERCICE 1 ##### *)
(* ###################### *)

(* Retourne la liste des sommets du graphe g*)
let liste_sommet g = 
	List.fold_left (fun a (s,_) -> a@[s]) [] g
;; 

(* Retourne la liste des successeurs du sommet n dans le graphe g *)
let rec liste_succ g n =
	match g with
		[] -> failwith "Can't find vertex."
		| (s,succ)::r -> if s=n 	then succ
									else liste_succ r n
;;

(* Retourne la liste des prédecesseurs du sommet n dans le graphe g (pour inverse_graphe) *)
let rec findPred n g =
	match g with
		[] -> []
		| (s,succ)::r -> if List.exists (fun p -> if p=n then true else false) succ
							then s::findPred n r
							else findPred n r
;;

(* Inverse le graphe g *)
let inverse_graphe g =
	List.fold_left (fun a (s,succ) -> a@[(s, findPred s g)]) [] g
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

(* Retirer les éléments de l1 dans l2 *)
let rec remove res l1 l2 =
	match l1 with
		| [] -> (List.rev res)
		| h::t -> 	if List.mem h l2 then
						remove res t l2
					else
						remove (h::res) t l2
;;

(* Retourne les CFC dans un tableau grâce à parcours_prof *)
let connexites graph =
	let suffixe = parcours_prof graph in
	let inverse_suffixe = parcours_prof (inverse_graphe graph) in
	let rec rConnexites (suff, invSuff) save res =
		match invSuff with
			| [] -> res
			| h::t -> 	if h=(List.hd suff) then
							rConnexites
							((remove [] suff (h::t))
							,(remove [] save (h::t))) 
							(remove [] save (h::t))
							[(List.rev (h::t))]@res
						else
							rConnexites
							(suff,t)
							save
							res
	in rConnexites (suffixe,inverse_suffixe) inverse_suffixe []
;;

(* ###################### *)
(* ##### EXERCICE 2 ##### *)
(* ###################### *)

(* Initialisation d'un jeu de données (vertex, i, num, seen)
	
	vertex: identifiant du sommet
	i: indice du sommet dans le parcours de Tarjan
	num: valeur "low link", pour regrouper les CFC entre eux
	seen: booléen indicateur de si on a visité le sommet ou non
 *)
let rec init graph =
	match graph with
		| [] -> []
		| (v,n)::t -> (v, 0, 0, false)::(init t)
;;

(* Retourne le minimum entre a et b *)
let min a b =
	if a<b then a
	else b 
;;

(* Retourne l'indice maximum dans data *)
let maxI data =
	let rec rmaxI d max =
		match d with
			| [] -> max
			| (_, i, _, _)::t -> 	if i>max then
										rmaxI t i
									else
										rmaxI t max
	in rmaxI data 0
;;

(* Retourne le num maximum dans data *)
let maxNum data =
	let rec rmaxNum d max =
		match d with
			| [] -> max
			| (_, _, num, _)::t ->	if num>max then
										rmaxNum t num
									else
										rmaxNum t max
	in rmaxNum data 0
;;

(* ##### SETTERS ##### *)
let rec setI data v i =
	match data with
		| [] -> failwith "Can't find the vertex."
		| (vertex, iOLD, num, seen)::t -> 	if v=vertex then (v, i, num, seen)::t
												else (vertex, iOLD, num, seen)::(setI t v i)
;;

let rec setNum data v n =
	match data with
		| [] -> failwith "Can't find the vertex."
		| (vertex, i, num, seen)::t ->	if v=vertex then (v, i, n, seen)::t
											else (vertex, i, num, seen)::(setNum t v n)
;;

let rec setSeen data v s =
	match data with
		| [] -> failwith "Can't find the vertex."
		| (vertex, i, num, seen)::t ->	if v=vertex then (v, i, num, s)::t
											else (vertex, i, num, seen)::(setSeen t v s)
;;

(* ##### GETTERS ##### *)
let rec getI data v =
	match data with
		| [] -> failwith "Can't find the vertex."
		| (vertex, i, _, _)::t -> 	if v=vertex then i
									else getI t v
;;

let rec getNum data v =
	match data with
		| [] -> failwith "Can't find the vertex."
		| (vertex, _, num, _)::t -> 	if v=vertex then num
										else getNum t v 
;;

let rec isSeen data v =
	match data with
		| [] -> failwith "Can't find the vertex."
		| (vertex, _, _, s)::t -> 	if v=vertex then s
									else isSeen t v
;;

(* Va faire récursivement le parcours de tarjan en sauvegardant les informations dans data *)
let rec rTarjan graph data v =
	let rec neighbours succ d =
		match succ with
			| [] -> d
			| h::t -> 	if (getI d h)=0 then
							let d = rTarjan graph d h in
							let d = setNum d h (min (getNum d v) (getNum d h)) in
							neighbours t d
						else
							if (isSeen d h) then
								let d = setNum d v (min (getNum d v) (getNum d h)) in
								neighbours t d
							else
								neighbours t d
	in neighbours (liste_succ graph v) 	(setI
											(setNum
												(setSeen
													data 
													v 
													true)
												v 
												((maxI data)+1)
											)
											v 
											((maxI data)+1)
										)
;;

(* Extrait les informations de data pour obtenir la liste des CFC *)
let format data =
	let rec rFormat n max res =
		let rec rFormat2 d li =
			match d with
				| [] -> rFormat (n+1) max ([li]@res)
				| (vertex, _, num, _)::t -> if num=n then (rFormat2 t (vertex::li))
											else (rFormat2 t li)
		in if(not (n=max)) then rFormat2 data [] else res
	in rFormat 0 ((maxNum data)+1) []
;;

(* Fonction Tarjan *)
let tarjan graph =
	let data = (init graph) in
	match data with
		| (vertex, i, num, seen)::t -> format (rTarjan graph data vertex)
		| [] -> failwith "Your graph doesn't have any vertex."
;;

let graphe1 = [	(1, [6;7;8]) ;
				(2, [1;4]) ;
				(3, [2]) ;
				(4, [3;5]) ;
				(5, [1]) ;
				(6, [5;7]) ;
				(7, []) ;
				(8, [6;7])]
;;

parcours_prof graphe1;;
parcours_prof (inverse_graphe graphe1);;
connexites graphe1;;
tarjan graphe1;;