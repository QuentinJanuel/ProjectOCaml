#load "graphics.cma";;
#load "point.cmo";;
#load "list_tri.cmo";;

open Graphics;;
open Point;;
open List_tri;;

let min_point p1 p2 = if p1.abs < p2.abs then true else false;;

let max_point p1 p2 = if p1.abs > p2.abs then true else false;;

let points_depart l = (min_list min_point l), (min_list max_point l);;

let rec points_droite l p1 p2 = match l with
	| [] -> []
	| x::y -> let c = (p2.abs-p1.abs)*(x.ord-p1.ord)-(p2.ord-p1.ord)*(x.abs-p1.abs) in
		if c<0 then x::(points_droite y p1 p2)
		else points_droite y p1 p2
;;

let equation_droite p1 p2 =
	let a = p2.ord-p1.ord and b = p1.abs-p2.abs in
		let c = -a*p1.abs-b*p1.ord in
			a, b, c
;;

(* let mypt1 = {abs=1; ord=1} and mypt2 = {abs=3; ord=2} in
	equation_droite mypt1 mypt2;; *)

let distance_droite a b c p =
	let sqr x = x*x in
		let numerateur = sqrt (float_of_int (sqr (a*p.abs+b*p.ord+c))) in
			let denominateur = sqrt (float_of_int (sqr a + sqr b)) in
				numerateur/.denominateur
;;

let distance_max p1 p2 pa pb = 
	let a, b, c = equation_droite p1 p2 in
		let dist_a = distance_droite a b c pa and dist_b = distance_droite a b c pb in
			if dist_a > dist_b then true else false
;;

let point_eloigne p1 p2 l =
	let comp x y = if distance_max p1 p2 x y then true else false in
		min_list comp l
;;

let points_egaux p1 p2 = p1 = p2;;

let rec ajoute_list_apres p1 p l = match l with
	| [] -> []
	| x::y -> if points_egaux x p1 then x::(p::y) else x::(ajoute_list_apres p1 p y)
;;

let rec findhull l p q enveloppe =
	match l with
	[] -> enveloppe
	| x::r -> let c = point_eloigne p q l in
		let enveloppe = ajoute_list_apres p c enveloppe in
		let enveloppe = findhull (points_droite l p c) p c enveloppe in
			findhull (points_droite l c q) c q enveloppe;;

let quickhull l =
	let l = suppr_doublons l in
	let (pa,pb) = points_depart l in
	let enveloppe = [pa; pb] in
	let enveloppe = findhull (points_droite l pa pb) pa pb enveloppe in
		findhull (points_droite l pb pa) pb pa enveloppe;;

let enveloppe_convexe g n =
	let l = g n in
		(vider ();
		set_color red;
		tracer_nuage l ;
		set_color blue;
		tracer_polygone(quickhull l));;

initialiser();;
enveloppe_convexe gen_rectangle 10000;;

(* let rec loop b = loop b;;

loop 1;; *)