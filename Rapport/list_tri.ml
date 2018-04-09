(* #load "hasard.cmo";;

Hasard.init_random ();;
Random.self_init ();; *)

(* ------------------------------------------------ *)
(* Tri par creation du maximum (Selection sort) *)

(* Renvoie le max de l par rapport à la fonction comp *)
let rec selectionne_max comp l = match l with
	| [] -> failwith "Cannot find a max in an empty list"
	| [x] -> x
	| x::y -> let c = selectionne_max comp y in
		if comp c x then x else c
;;

(* Supprime la premiere occurence de x dans l *)
let rec supprime x l = match l with
	| [] -> []
	| a::b -> if a = x then b else a::(supprime x b)
;;

let ajoute_fin x l = l@[x];;

(* Fonction de tri *)
let rec tri_creation_max comp l =
	if l = [] then l else
	let max = selectionne_max comp l in
		let temp_list = supprime max l in
			ajoute_fin max (tri_creation_max comp temp_list)
;;

(* ------------------------------------------------ *)
(* Tri par partition-fusion (Merge sort) *)

(* Renvoie un couple de la forme ([l0;l2;l4;...], [l1;l3;l5;...]) *)
let partitionne l =
	let rec aux p l = match l with
	| [] -> []
	| [x] -> if p then [x] else []
	| x::(y::z) -> if p then x::(aux true z) else y::(aux false z)
	in
	((aux true l), (aux false l))
;;

(* Fusionne deux listes triées en conservant l'ordre *)
let rec fusionne comp l1 l2 = match l1, l2 with
	| a::b, x::y -> if comp a x then a::(fusionne comp b l2) else x::(fusionne comp l1 y)
	| _, _ -> l1@l2
;;

(* Triage *)
let rec tri_partition_fusion comp l =
	let (a, b) = partitionne l in
	if List.length l <= 1 then l
	else
		let tried1 = tri_partition_fusion comp a and
			tried2 = tri_partition_fusion comp b in
				fusionne comp tried1 tried2
;;

(* ------------------------------------------------ *)
(* Tri par arbre binaire de recherche *)

(* Type AB *)
type 'a arbreBinaire =
	| Noeud of 'a * 'a arbreBinaire * 'a arbreBinaire
	| ArbreVide
;;

(* Insere un noeud dans un ABR *)
let rec insere_noeud comp x a = match a with
	| Noeud (n, l, r) -> if comp x n then Noeud(n, insere_noeud comp x l, r)
		else Noeud(n, l, insere_noeud comp x r) 
	| ArbreVide -> Noeud(x, ArbreVide, ArbreVide)
;;

(* Insere une liste de noeuds dans un ABR *)
let rec insere_liste_noeuds comp l a = match l with
	| [] -> a
	| x::y -> (insere_liste_noeuds comp y (insere_noeud comp x a))
;;

(* Retourne une liste résultante d'un parcours de gauche à droite d'un ABR *)
let rec parcours_arbre a = match a with
	| ArbreVide -> []
	| Noeud (n, l, r) -> (parcours_arbre l)@(n::(parcours_arbre r))
;;

(* Triage *)
let rec tri_par_abr comp l = match l with
	| [] -> []
	| x::y -> let abr = Noeud(x, ArbreVide, ArbreVide) in
		parcours_arbre (insere_liste_noeuds comp y abr)
;;

(* ------------------------------------------------ *)
(* Choix de la meilleure fonction de tri *)

let tri comp l = tri_partition_fusion comp l;;

(* ------------------------------------------------ *)
(* Autres fonctions *)

let min_list comp l = selectionne_max (fun x y -> not(comp x y)) l;;

let rec suppr_doublons l =
	match l with
	| [] -> l
	| x::subL ->
		let l2 = suppr_doublons subL in
			if List.mem x subL then l2 else x::l2
;;