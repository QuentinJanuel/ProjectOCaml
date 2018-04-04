let rec selectionne_max comp l =
	match l with
	| [] -> failwith "Calling 'selectionne_max' with an empty list"
	| x::subList ->
		if List.length subList = 0 then x
		else
			let x2 = selectionne_max comp subList in
				if comp x x2 then x2 else x
;;

selectionne_max (<) [2; 8; 4; 1];;
