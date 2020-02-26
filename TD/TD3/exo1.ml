type 'a arbre = Vide | Arbrebin of 'a * 'a arbre * 'a arbre;;

let rec inserer a x=
	match a with
		Vide -> Arbrebin(x,Vide,Vide)
	| 	Arbrebin(n,g,d) -> 
			if x>n 	then Arbrebin(n,g,inserer d x)
					else Arbrebin(n,inserer g x ,d)
;;

let list_arbre l =
	List.fold_left inserer Vide l
;;

let arbre_list a =
;;