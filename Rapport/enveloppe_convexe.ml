#directory "Modules";;
#load "graphics.cma";;
#load "point.cmo";;
#load "list_tri.cmo";;
open Graphics;;
open Point;;
open List_tri;;

let min_point p1 p2 = p1.abs < p2.abs;;

let max_point p1 p2 = p1.abs > p2.abs;;

let points_depart l =
	let pa = min_list min_point	l
	and pb = min_list max_point l in
		pa, pb
;;

let rec points_droite l p1 p2 =
	let est_a_droite p p1 p2 =
		(p2.abs-p1.abs)*(p.ord-p1.ord)-(p2.ord-p1.ord)*(p.abs-p1.abs) > 0
	in
		match l with
		| [] -> []
		| x::subL ->
			if est_a_droite x p1 p2 then
				x::(points_droite subL p1 p2)
			else
				points_droite subL p1 p2
;;

let equation_droite pa pb =
	let a = pb.ord-pa.ord
	and b = pa.abs-pb.abs in
		let c = -a*pa.abs-b*pa.ord in
			a, b, c
;;

let distance_droite a b c p =
	let num = abs (a*p.abs+b*p.ord+c)
	and den = sqrt (float_of_int (a*a+b*b)) in
		(float_of_int num)/.den
;;

let distance_max p1 p2 pa pb =
	let a, b, c = equation_droite p1 p2 in
		let da = distance_droite a b c pa
		and db = distance_droite a b c pb in
			da > db
;;

let rec point_eloigne p1 p2 l =
	let comp a b = distance_max p1 p2 a b in
		min_list comp l
;;

let points_egaux p1 p2 = p1 = p2;;
(* 
Vous vouliez probablement que l'on fasse
(p1.abs = p2.abs) && (p1.ord = p2.ord)
mais OCaml supporte déjà l'opérateur = avec le type point,
alors autant le laisser tel quel.
 *)

let rec ajoute_list_apres p1 p l =
	match l with
	| [] -> []
	| x::subL ->
		if points_egaux p1 x then
			[p1; p]@subL
		else
			x::(ajoute_list_apres p1 p subL)
;;

let rec findhull l p q enveloppe =
	match l with
	| [] -> enveloppe
	| _ ->
		let c = point_eloigne p q l in
			let enveloppe = ajoute_list_apres p c enveloppe in
				let enveloppe = findhull (points_droite l p c) p c enveloppe in
					findhull (points_droite l c q) c q enveloppe
;;

let quickhull l =
	let l = suppr_doublons l in
		let pa, pb = points_depart l in
			let enveloppe = findhull (points_droite l pa pb) pa pb [pa; pb] in
				findhull (points_droite l pb pa) pb pa enveloppe
;;

let enveloppe_convexe g n =
	let l = g n in
		(
			vider ();
			set_color blue;
			tracer_nuage l;
			set_color black;
			tracer_polygone (quickhull l)
		)
;;

let rec attendre sec =
	let rec aux sec sT =
		if sec < (Sys.time ())-.sT then
			true
		else
			aux sec sT
	in
		aux (float_of_int sec) (Sys.time ())
;;

let rec gen_tout gens n =
	let n = n mod List.length gens in
		(
			enveloppe_convexe (List.nth gens n) 4000;
			attendre 1;
			gen_tout gens (n+1)
		)
;;

initialiser ();;

let gens = [
	gen_rectangle;
	gen_cercle;
	gen_papillon;
	gen_cervolant;
	gen_soleil;
	gen_poisson
];;

gen_tout gens 0;;
