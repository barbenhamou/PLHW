open Ast_sol
open Nos_sol
open Semantics_sol

Random.self_init ()

let randimizedArray n max = Array.init n (fun _ -> Random.int max)

let swap 

let run_bubble_sort n =
  Comp (
    Ass ("i", 1),
    While (Gte (Num n, Var ""), )
  )
