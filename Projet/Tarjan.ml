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

(*
*)
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

let parcours_prof graph =
    let rec rExplorer visited node =
        if not (List.mem node visited) then
            let s = liste_succ graph node in
                List.fold_left rExplorer (node::visited) s
        else
            visited
    in List.fold_left rExplorer [] (liste_sommet graph)
;;

(*
let rec parcours_profondeur g seen tosee =
	match tosee with
		[] -> []
	| t::r -> if List.mem t seen
				then parcours_profondeur g seen r
				else match t with
					(s,succ) -> (parcours_profondeur g s::seen succ@(tl toseen))@[s]
;;


let parcours_profondeur out = function
		(s,succ)::r -> if
*)
(*
let parcours graphe = 
	let rec visite listeDejaVisit (s,li) =
		if List.mem s listeDejaVisit
			then listeDejaVisit
			else List.fold_left (fun base ei -> visite (s::base) (List.nth graphe ei)) listeDejaVisit li 
	in visite [] (List.nth graphe 1);;

let parcours_prof g =
*)


(*
Depth Probe
=> get vertex_label
=> get 
check vlabel of CS isn't in record

(int * int list) list -> (int * int list)

-> int list  
*)
(*
Essentially returns the first of a couple
let rec vertex_label = function
	(s,_) -> s
;;

let rec get_vertex = function
	(s,succ)::r -> function 
					n -> if n=s then (s,succ)
								else get_vertex r n
| [] -> failwith "404 vertex not found"
;;

let rec get_vertex graph n =
	match graph with
		s::r -> if n=vertex_label s 
					then s
					else get_vertex r n
;;

let rec a f l = 
	match l with
		s::r ->
;; 

let rec depthProbe (s,succ) g hm p =
	if (List.mem s hm) then []
	
	else if (List.mem s p) then p

	else
		
		s::( List.fold_left (depthProbe (get_vertex g N ) g s::hm p) )



let rec depthProbe s g hm p =
	if ()List.mem
;;
*)