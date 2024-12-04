(* Define the binary tree type *)
type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* Define the comparator_tree function *)
let rec comparator_tree tree elem comparator =
  match tree with
  | Empty -> Node (elem, Empty, Empty)
  | Node (value, left, right) ->
      if comparator elem value <= 0 then
        Node (value, comparator_tree left elem comparator, right)
      else
        Node (value, left, comparator_tree right elem comparator);;
