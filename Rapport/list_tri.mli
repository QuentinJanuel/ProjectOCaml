(* "tri comp l" retourne la liste l triée selon l'ordre comp *)
val tri : ('a -> 'a -> bool) -> 'a list -> 'a list

(* "min_list comp l" retourne le minimum de la liste l selon l'ordre comp *)
val min_list : ('a -> 'a -> bool) -> 'a list -> 'a

(* "suppr_doublons l" retourne la liste l sans ses éventuels doublons *)
val suppr_doublons : 'a list -> 'a list

