#load "hasard.cmo";;

Hasard.init_random ();;

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
			let subLT = tri_creation_max comp subL in
				ajoute_fin max subLT
;;

let myList = Hasard.random_list 10 10;;
tri_creation_max (<) myList;;
tri_creation_max (>) myList;;

