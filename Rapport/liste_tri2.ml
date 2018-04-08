#load "hasard.cmo";;

Hasard.init_random ();;
Random.self_init ();;

(* Tri par creation du maximum *)

let rec selectionne_max comp l = match l with
	| [] -> failwith "Cannot find a max in an empty list"
	| [x] -> x
	| x::y -> let c = selectionne_max comp y in
		if comp c x then x else c
;;

(* selectionne_max (<) [1; 3; 8; 0];; *)

let rec supprime x l = match l with
	| [] -> []
	| a::b -> if a = x then b else a::(supprime x b)
;;
(* 
supprime 2 [1;2;4;1];; *)

let ajoute_fin x l = l@[x];;

(* ajoute_fin 5 [4;2;5;1];; *)

let rec tri_creation_max comp l =
	if l = [] then l else
	let max = selectionne_max comp l in
		let temp_list = supprime max l in
			ajoute_fin max (tri_creation_max comp temp_list)
;;

tri_creation_max (>) (Hasard.random_list 10 10);;

 (* Tri par partition-fusion *)

let partitionne l =
	let rec aux p l = match l with
	| [] -> []
	| [x] -> if p then [x] else []
	| x::(y::z) -> if p then x::(aux true z) else y::(aux false z)
	in
	((aux true l), (aux false l))
;;

let rec fusionne comp l1 l2 = match l1, l2 with
	| a::b, x::y -> if comp a x then a::(fusionne comp b l2) else x::(fusionne comp l1 y)
	| _, _ -> l1@l2
;;

let rec tri_partition_fusion comp l =
	let (a, b) = partitionne l in
	if List.length l <= 1 then l
	else
		let tried1 = tri_partition_fusion comp a and
			tried2 = tri_partition_fusion comp b in
				fusionne comp tried1 tried2
;;

tri_partition_fusion (<) [1; 5; 3; 0; 9; 5];;

 (* Tri par arbre binaire de recherche *)

type 'a arbreBinaire =
	| Noeud of 'a * 'a arbreBinaire * 'a arbreBinaire
	| ArbreVide
;;

let rec insere_noeud comp x a = match a with
	| Noeud (n, l, r) -> if comp x n then Noeud(n, insere_noeud comp x l, r)
		else Noeud(n, l, insere_noeud comp x r) 
	| ArbreVide -> Noeud(x, ArbreVide, ArbreVide)
;;

let rec insere_liste_noeuds comp l a = match l with
	| [] -> a
	| x::y -> (insere_liste_noeuds comp y (insere_noeud comp x a))
;;

let rec parcours_arbre a = match a with
	| ArbreVide -> []
	| Noeud (n, l, r) -> (parcours_arbre l)@(n::(parcours_arbre r))
;;

(* let a = Noeud(4,
Noeud(2,ArbreVide,ArbreVide),
Noeud(7,
Noeud(5,ArbreVide,ArbreVide),
Noeud(9,ArbreVide,ArbreVide)));;

parcours_arbre a;;

parcours_arbre (insere_liste_noeuds (<) [3; 6; 9] a);; *)

let rec tri_par_abr comp l = match l with
	| [] -> []
	| x::y -> let abr = Noeud(x, ArbreVide, ArbreVide) in
		parcours_arbre (insere_liste_noeuds comp y abr)
;;

tri_par_abr (<) [2; 5; 8; 7; 9; 6; 0; 4];;
