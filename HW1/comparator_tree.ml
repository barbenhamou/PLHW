type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec comparator_tree tree e func =  match tree with
  | Empty -> Node(e, Empty, Empty)
  | Node (value, left, right) -> 
      if (func value e) = true then
        Node(value, comparator_tree left e func, right)
      else 
        Node(value, left, comparator_tree right e func);;



