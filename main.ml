open Lang
open Evaluator

let () =
  let expr = EAppl (EVar "id", (EAppl (EAppl (EVar "+", EVal (VInt 9)),
                                       EVal (VInt 10)))) in
  let expr2 = EAppl (EVar "+", EVal (VInt 9)) in
  let ev = eval expr initial_env in
  let ev2 = eval expr2 initial_env in
  let _ = print_endline (string_of_val ev) in
  print_endline (string_of_val ev2)
