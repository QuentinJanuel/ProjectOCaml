#load "hasard.cmo";;

Hasard.init_random ();;


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

let myList = Hasard.random_list 10 10;;
tri_creation_max (<) myList;;
tri_creation_max (>) myList;;


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

let myList = Hasard.random_list 10 10;;
tri_partition_fusion (<) myList;;
tri_partition_fusion (>) myList;;
