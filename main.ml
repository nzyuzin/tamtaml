open Evaluator

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let () = print_string "=>  "; flush stdout in
      let expr = Parser.main Lexer.token lexbuf in
      let result = eval expr initial_env in
      print_endline (string_of_val result); flush stdout
    done
  with Lexer.Eof ->
    exit 0
