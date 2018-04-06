#directory "Modules";;
#load "hasard.cmo";;

(* Hasard.init_random ();; *)
Random.self_init ();;

(* Tri par creation du maximum *)

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

 (* Tri par partition-fusion *)

let partitionne l =
	let rec aux p l = match l with
	| [] -> []
	| [x] -> if p then [x] else []
	| x::(y::z) -> if p then x::(aux true z) else y::(aux false z)
	in
	((aux true l), (aux false l))
;;

(* let rec fusionne comp l1 l2 =
	let len1 = List.length l1 and len2 = List.length l2 in
		if len1 = 0 then l2 else if len2 = 0 then l1 
	let f1 = List.nth l1 0 and f2 = List.nth l2 0
	and r1 = (List.tl l1) and r2 = (List.tl l2) in
		if comp f1 f2 then f1::(f2::(fusionne comp r1 r2)) else f2::(f1::(fusionne comp r1 r2))
;; *)

let rec fusionne comp l1 l2 = match l1, l2 with
	| a::b, x::y -> if comp a x then a::(fusionne comp b l2) else x::(fusionne comp l1 y)
	| _, _ -> l1@l2
;;

fusionne (<) [1; 4; 6; 7] [2; 5; 8];;