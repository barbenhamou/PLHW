open Ast
open Nos
open Semantics

let gen_x i = "x" ^ string_of_int i

(* Generate n variables with random values *)
let rec generate n s =
  if n = 0 then s
  else generate (n - 1) (update (gen_x n) (Num (Random.int 3200)) s)

(* Swap operation *)
let swap vi vj =
  Comp (
    Ass ("temp", Var vi),
    Comp (
      Ass (vi, Var vj),
      Ass (vj, Var "temp")
    )
  )

(* One pass of bubbling up with a sorted flag *)
let rec bubbling_up_with_flag i n =
  if i < n then
    Comp (
      If (
        Neg (Gte (Var (gen_x (i + 1)), Var (gen_x i))),
        Comp (
          swap (gen_x i) (gen_x (i + 1)),
          Ass ("sorted", Num 1)  (* Set sorted to 0 if a swap is made *)
        ),
        Skip
      ),
      bubbling_up_with_flag (i + 1) n
    )
  else
    Skip

(* Bubble sort logic with the boolean improvement *)
let rec bubble_sort_logic_with_flag n times =
  if times <= 0 then Skip
  else
    Comp (
      Ass ("sorted", Num 0),  (* Assume the array is sorted *)
      Comp (
        bubbling_up_with_flag 1 n,  (* Perform bubbling up *)
        If (
          Neg (Gte (Var "sorted", Num 1)),  (* If sorted is 0, terminate early *)
          Skip,
          bubble_sort_logic_with_flag n (times - 1)  (* Continue to the next pass *)
        )
      )
    )

(* Initialize the state dynamically with n variables *)
let initialize_state n =
  let state = generate n default_state in
  let state = update "sorted" (Num 1) state in
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

  let cmd = bubble_sort_logic_with_flag n n in
  let final = nos (cmd, init) in

  Printf.printf "The sorted variables:\n";
  print_state n final

let () = run_bubble_sort 10
