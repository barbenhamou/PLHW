open Ast
open Nos
open Semantics

(* Generate variable names dynamically *)
let gen_var i = "v" ^ string_of_int i

(* Swap operation in While *)
let swap vi vj =
  Comp (
    Ass ("temp", Var vi),
    Comp (
      Ass (vi, Var vj),
      Ass (vj, Var "temp")
    )
  )

(* Initialize variables with random values *)
let rec initialize i n =
  if i > n then Skip
  else Comp (Ass (gen_var i, Num (Random.int 100)), initialize (i + 1) n)

(* Print variables in the While program *)
let rec printI i n acc =
  if i > n then acc
  else printI (i + 1) n (Comp (Ass ("out", Var (gen_var i)), acc))

(* Create and run Bubble Sort *)
let run_bubble_sort n =
  let initialization = initialize 1 n in
  let print_initial = printI 1 n Skip in
  let sorting_logic =
    While (
      Neg (Gte (Var "i", Num n)),
      Comp (
        Ass ("j", Num 1),
        While (
          Neg (Gte (Var "j", Sub (Num n, Add (Var "i", Num 1)))),
          Comp (
            If (
              Gte (
                Var (gen_var (solve_a (Var "j") default_state)),
                Var (gen_var (solve_a (Add (Var "j", Num 1)) default_state))
              ),
              swap (gen_var (solve_a (Var "j") default_state))
                   (gen_var (solve_a (Add (Var "j", Num 1)) default_state)),
              Skip
            ),
            Ass ("j", Add (Var "j", Num 1))
          )
        )
      )
    )
  in
  Comp (
    initialization,  (* Initialization of the variables *)
    Comp (
      print_initial,
      Comp (
        sorting_logic,
        print_initial
      )
    )
  )

(* Print variables as a row in OCaml *)
let rec print_vars i n state =
  if i > n then Printf.printf "\n"
  else (
    let var = gen_var i in
    Printf.printf "%d " (state var);
    print_vars (i + 1) n state
  )

(* Run the Bubble Sort program *)
let test6 = run_bubble_sort 6 ;;
let initial_state = default_state ;;
let final_state = nos (test6, initial_state) ;;

(* Print final sorted state *)
Printf.printf "Final sorted state: " ;;
print_vars 1 6 final_state ;;
