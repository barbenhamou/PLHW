open Ast
open Nos
open Semantics

(* Generate variable names for array elements (x1, x2, ..., xn) *)
let gen_x i = "x" ^ string_of_int i

(* Add n randomly generated vars x1, x2, ..., xn and return the new state *)
let rec generate n s =
  if n = 0 then s
  else generate (n - 1) (update (gen_x n) (Num (Random.int 1500)) s)

(* Swap operation for two adjacent variables *)
let swap vi vj =
  Comp (
    Ass ("temp", Var vi),
    Comp (
      Ass (vi, Var vj),
      Ass (vj, Var "temp")
    )
  )

(* Generate the bubble sort inner loop logic for comparison and swapping *)
let rec bubbling_up i n =
  if i < n then
    Comp(
      If (
        Neg (Gte (Var (gen_x (i + 1)), Var (gen_x i))),
        swap (gen_x i) (gen_x (i + 1)),
        Skip
      ),
      bubbling_up (i + 1) n
    )
  else
    Skip
    

(* Generate the bubble sort logic: multiple passes through the array *)
let rec bubble_sort_pass n rounds =
  if rounds <= 0 then Skip
  else
    Comp (
      bubbling_up 1 n,  (* Compare and swap adjacent elements in this pass *)
      bubble_sort_pass n (rounds - 1)  (* Continue to the next pass *)
    )

(* Initialize the state dynamically with n variables *)
let initialize_state n =
  let state = generate n default_state in
  (* Initializing additional control variables *)
  let state = update "n" (Num n) state in
  let state = update "i" (Num 1) state in
  let state = update "temp" (Num 0) state in
  state

(* Print the values of the variables in the state *)
let print_state n state =
  for i = 1 to n do
    let var_name = gen_x i in
    Printf.printf "%d  " (state var_name)
  done;
  Printf.printf "\n"

(* Run the bubble sort algorithm with n variables *)
let run_bubble_sort n =
  (* Initialize the state with random values for n variables *)
  let init = initialize_state n in
  Printf.printf "Initialization of n variables:\n";
  print_state n init;

  (* Generate and solve the bubble sort statement with the initialized state *)
  let sort_stmt = bubble_sort_pass n n in
  let final = nos (sort_stmt, init) in

  (* Print the sorted state *)
  Printf.printf "The sorted variables:\n";
  print_state n final

(* Execute the function *)
let () = run_bubble_sort 6
