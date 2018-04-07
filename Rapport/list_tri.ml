(* 
Il n'est pas possible de compiler en .cmo un code en OCaml comportant une directive.
Les tests étant finis, nous n'avons plus besoin de charger le module 'hasard'.
 *)
(* 
#directory "Modules";;
#load "hasard.cmo";;
Hasard.init_random ();;
let ma_liste = Hasard.random_list 10 10;;
 *)


(* Tri par création du maximum *)

let rec selectionne_max comp l =
	match l with
	| [] -> failwith "Calling 'selectionne_max' with an empty list"
	| x::subL ->
		if List.length subL = 0 then x
		else
			let x2 = selectionne_max comp subL in
				if comp x x2 then x2 else x
;;

let rec supprime x l =
	match l with
	| [] -> l
	| x2::subL ->
		if x = x2 then subL
		else x2::(supprime x subL)
;;

let ajoute_fin x l = l@[x];;

let rec tri_creation_max comp l =
	if l = [] then l else
	let max = selectionne_max comp l in
		let subL = supprime max l in
			let sorted = tri_creation_max comp subL in
				ajoute_fin max sorted
;;

(* Tests *)

(* 
tri_creation_max (<) ma_liste;;
tri_creation_max (>) ma_liste;;
 *)


(* Tri par partition-fusion *)

let partitionne l =
	let rec getHalf even l =
		match l with
		| [] -> []
		| [x] -> if even then [] else [x]
		| x::(y::subL) ->
			let half = getHalf even subL in
				if even then y::half else x::half
	in
		(getHalf true l), (getHalf false l)
;;

let rec fusionne comp l1 l2 =
	match l1, l2 with
	| x1::subL1, x2::subL2 ->
		if comp x1 x2 then
			x1::(fusionne comp subL1 l2)
		else
			x2::(fusionne comp l1 subL2)
	| _, _ -> l1@l2
;;

let rec tri_partition_fusion comp l =
	let l1, l2 = partitionne l in
		if (List.length l1)+(List.length l2) < 2 then
			l1@l2
		else
			let sorted1 = tri_partition_fusion comp l1
			and sorted2 = tri_partition_fusion comp l2 in
				fusionne comp sorted1 sorted2
;;

(* Tests *)

(*
tri_partition_fusion (<) ma_liste;;
tri_partition_fusion (>) ma_liste;;
 *)


(* Tri par arbre binaire de recherche *)

type 'a arbreBinaire =
	| Noeud of 'a * 'a arbreBinaire * 'a arbreBinaire
	| ArbreVide
;;

let rec insere_noeud comp x a =
	match a with
	| ArbreVide -> Noeud(x, ArbreVide, ArbreVide)
	| Noeud(x2, left, right) ->
		if comp x x2 then
			Noeud(x2, insere_noeud comp x left, right)
		else
			Noeud(x2, left, insere_noeud comp x right)
;;

let rec insere_liste_noeuds comp l a =
	match l with
	| [] -> a
	| x::subL ->
		let a2 = insere_noeud comp x a in
			insere_liste_noeuds comp subL a2
;;

let rec parcours_arbre a =
	match a with
	| ArbreVide -> []
	| Noeud(x, left, right) -> (parcours_arbre left)@[x]@(parcours_arbre right)
;;

let tri_par_abr comp l = parcours_arbre (insere_liste_noeuds comp l ArbreVide);;

(* Tests *)

(* 
tri_par_abr (<) ma_liste;;
tri_par_abr (>) ma_liste;;
 *)


(* Tests de rapidité des différents algorithmes *)

(* 
let rec liste_de_listes l n =
	if n = 0 then [] else
		let el = Hasard.random_list 10 l in
			el::(liste_de_listes l (n-1))
;;
let n = 1;;
let mes_listes = liste_de_listes 1000 n;;
let tests_tri algo =
	let temps_debut = Sys.time () in
		let rec tris_boucle p algo =
			if p = 0 then
				0
			else
				let ma_liste = List.nth mes_listes (p-1) in
					let _ = algo (<) ma_liste in
						tris_boucle (p-1) algo
		in
		let _ = tris_boucle n algo in
			Sys.time ()-.temps_debut
;;
let max    = tests_tri tri_creation_max;;
let fusion = tests_tri tri_partition_fusion;;
let arbre  = tests_tri tri_par_abr;;
 *)


(* Suite *)

let tri = tri_partition_fusion;;

let min_list comp l =
	let nComp a b = not (comp a b) in
		selectionne_max nComp l
;;

let rec suppr_doublons l =
	match l with
	| [] -> l
	| x::subL ->
		let l2 = suppr_doublons subL in
			if List.mem x subL then l2 else x::l2
;;
