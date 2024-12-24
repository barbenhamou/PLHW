[@@@ocaml.warning "-8"];;

let rec nos (o, s) = match o with
  | Ast.Ass (v, e) -> Semantics.update v e s
  | Ast.Skip -> s
  | Ast.Comp (s1, s2) -> nos (s2, (nos (s1, s)))
  | Ast.If (b, s1, s2) -> if (Semantics.solve_b b s) = "tt" then (nos (s1, s)) else (nos (s2, s))
  | Ast.While (b, s1) -> let rec loop s = if (Semantics.solve_b b s) = "tt" then loop (nos (s1, s)) else ( s ) in loop s;;

(* tests *) 

print_string "x = ";;
print_int (let new_state = nos (Ast.test1, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test2, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test3, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "x");;
print_endline "";;

print_string "y = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "y");;
print_endline "";;

let new_state = nos (Ast.test5, Semantics.default_state);;
print_string "a = ";;
print_int (new_state "a");;
print_endline "";;

print_string "b = ";;
print_int (new_state "b");;
print_endline "";;

print_string "c = ";;
print_int (new_state "c");;
print_endline "";;

