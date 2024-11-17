type bool_expr =
    | Var  of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec evaluate bool1 bool2 var1 var2 exp = match exp with
  | Var a -> if a = var1 then bool1 else bool2
  | Not e -> not (evaluate bool1 bool2 var1 var2 e)
  | And (e1, e2) -> (evaluate bool1 bool2 var1 var2 e1) && (evaluate bool1 bool2 var1 var2 e2)
  | Or (e1, e2) -> (evaluate bool1 bool2 var1 var2 e1) || (evaluate bool1 bool2 var1 var2 e2);;

let table_two var1 var2 exp =
  (true, true, evaluate true true var1 var2 exp)
  :: (true, false, evaluate true false var1 var2 exp)
  :: (false, true, evaluate false true var1 var2 exp)
  :: (false, false, evaluate false false var1 var2 exp) :: [];;