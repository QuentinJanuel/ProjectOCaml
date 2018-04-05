#load "hasard.cmo";;

Hasard.init_random ();;
Random.self_init ();;

let rec selectionne_max comp l = match l with
	| [] -> failwith "Cannot find a max in an empty list"
	| [x] -> x
	| x::y -> let c = selectionne_max comp y in
		if comp c x then x else c
;;

selectionne_max (<) [1; 3; 8; 0];;

let rec supprime x l = match l with
	| [] -> []
	| a::b -> if a = x then b else a::(supprime x b)
;;

supprime 2 [1;2;4;1];;

let ajoute_fin x l = l@[x];;

ajoute_fin 5 [4;2;5;1];;

let rec tri_creation_max comp l =
	if l = [] then l else
	let max = selectionne_max comp l in
		let temp_list = supprime max l in
			ajoute_fin max (tri_creation_max comp temp_list)
;;

 tri_creation_max (>) (Hasard.random_list 10 100);;