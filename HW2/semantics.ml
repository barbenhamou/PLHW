(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with
  | Ast.Num n -> n 
  | Ast.Var v -> s v
  | Ast.Add (a, b) -> (solve_a a s) + (solve_a b s)
  | Ast.Sub (a, b) -> (solve_a a s) - (solve_a b s)
  | Ast.Mult (a, b) -> (solve_a a s) * (solve_a b s)
  | Ast.Shr (a, b) -> 
      let base = solve_a a s in
      let power = solve_a b s in
      if power > 0 then solve_a (Ast.Shr (Ast.Num (base / 2), Ast.Num (power - 1))) s
      else base
  | Ast.Shl (a, b) -> 
      let base = solve_a a s in
      let power = solve_a b s in
      if power > 0 then solve_a (Ast.Shl (Ast.Num (base * 2), Ast.Num (power - 1))) s
      else base

 (* solve_b: bexp -> state -> bool *) 
 let rec solve_b e s = match e with
 | Ast.True -> "tt"
 | Ast.False -> "ff"
 | Ast.Aeq (a, b) -> if (solve_a a s) = (solve_a b s) then "tt" else "ff"
 | Ast.Beq (a, b) -> if (solve_b a s) = (solve_b b s) then "tt" else "ff"
 | Ast.Gte (a, b) -> if (solve_a a s) >= (solve_a b s) then "tt" else "ff"
 | Ast.Neg b -> if (solve_b b s) = "tt" then "ff" else "tt"
 | Ast.And (a, b) -> if (solve_b a s) = "tt" && (solve_b b s) = "tt" then "tt" else "ff"


(* state update : to get a new state *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *) 
 raise (NotFound "undefined variable");; 

 (* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 
