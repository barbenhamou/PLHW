(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with
  | Ast.Num n -> n 
  | Ast.Var v -> s v
  | Ast.Add (a, b) -> (solve_a a s) + (solve_a b s)
  | Ast.Sub (a, b) -> (solve_a a s) - (solve_a b s)
  | Ast.Mult (a, b) -> (solve_a a s) * (solve_a b s);;

 (* solve_b: bexp -> state -> bool *) 
 let rec solve_b e s = match e with
  | Ast.True -> true
  | Ast.False -> false
  | Ast.Aeq (a, b) -> (solve_a a s) = (solve_a b s)
  | Ast.Beq (a, b) -> (solve_b a s) = (solve_b b s)
  | Ast.Gte (a, b) -> (solve_a a s) >= (solve_a b s)
  | Ast.Neg b -> not (solve_b b s)
  | Ast.And (a, b) -> (solve_b a s) && (solve_b b s)


(* state update : to get a new state *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *) 
 raise (NotFound "undefined variable");; 

 (* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 