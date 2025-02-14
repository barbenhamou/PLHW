let rec split_at n lst =
  match (n, lst) with
    | (_, []) -> ([], []) 
    | (0, _) -> ([], lst) 
    | (n, x :: y) -> let (left, right) = split_at (n - 1) y in (x :: left, right) 

let insert_at e i lst =
  let (left, right) = split_at i lst in
    left @ [e] @ right 

let insert_none_at index lst = insert_at "None" index lst;;