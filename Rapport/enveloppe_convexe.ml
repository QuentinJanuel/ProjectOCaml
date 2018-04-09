#load "graphics.cma";;
#load "point.cmo";;
#load "list_tri.cmo";;

open Graphics;;
open Point;;
open List_tri;;

let min_point p1 p2 = if p1.abs < p2.abs then true else false;;

let max_point p1 p2 = if p1.abs > p2.abs then true else false;;

let points_depart l = (min_list min_point l), (min_list max_point l);;

