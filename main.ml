open Evaluator

let print_prompt () = print_string "=>  "; flush stdout
let print_response str = print_endline ("- : " ^ str)
let print_error err = print_endline err; flush stdout

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let () = print_prompt () in
      let expr = Parser.main Lexer.token lexbuf in
      try
        let result = eval expr initial_env in
        print_response (string_of_val result);
        print_endline ""; flush stdout
      with
      | ExecutionError str -> print_error str
    done
  with Util.Eof ->
    print_endline ""; exit 0
