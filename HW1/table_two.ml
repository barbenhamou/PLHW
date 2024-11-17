type bool_expr =
    | Var  of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec evaluate = fun bool1 bool2 var1 var2 exp -> match exp with
  | Var(a) -> if a = var1 then bool1 else bool2
  | Not(e) -> if evaluate bool1 bool2 var1 var2 e = true then false else true
  | And(e1, e2) -> if evaluate bool1 bool2 var1 var2 e1 = true && evaluate bool1 bool2 var1 var2 e2 = true then true else false
  | Or(e1, e2) -> if evaluate bool1 bool2 var1 var2 e1 = true || evaluate bool1 bool2 var1 var2 e2 = true then true else false;;

let rec table_two = fun var1 var2 exp ->
  (true, true, evaluate true true var1 var2 exp)
  :: (true, false, evaluate true false var1 var2 exp)
  :: (false, true, evaluate false true var1 var2 exp)
  :: (false, false, evaluate false false var1 var2 exp) :: [];;