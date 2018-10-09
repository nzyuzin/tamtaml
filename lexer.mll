{
open Parser (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
| [' ' '\t'] { token lexbuf } (* skip blanks *)
| "fun" { FUN }
| "->" { FUNARROW }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| ['\n' ] { EOL }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['a'-'z']+ as lxm { IDENT(lxm) }
| ['+'] as lxm { IDENT(String.make 1 lxm) }
| eof { raise Eof }
