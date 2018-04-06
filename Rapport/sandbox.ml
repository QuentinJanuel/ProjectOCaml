#directory "Modules";;
#load "hasard.cmo";;
#load "list_tri.cmo";;

Random.self_init ();;

let myList = Hasard.random_list 10 100;;

List_tri.tri (<) myList;;
List_tri.tri (>) myList;;

List_tri.suppr_doublons myList;;

List_tri.min_list (<) myList;;
List_tri.min_list (>) myList;;
