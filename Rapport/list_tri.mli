(* Fonction de tri suivant un fonction de comparaison *)
val tri : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Renvoie le minimum d'une liste en fonction d'une fonction de comparaison *)
val min_list : ('a -> 'a -> bool) -> 'a list -> 'a

(* Supprime tous les doublons d'un liste *)
val suppr_doublons : 'a list -> 'a list