{
open Parser (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
| [' ' '\t'] { token lexbuf } (* skip blanks *)
| "fun" { FUN }
| "let" { LET }
| "in" { IN }
| "=" { EQUALS }
| "->" { FUNARROW }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { PLUS }
| '-' { MINUS}
| '*' { MULT }
| '/' { DIV }
| '"' ['a'-'z' 'A'-'Z' '0'-'9' ' ']+ '"' as lxm { STRING(lxm) }
| ['\n' ] { EOL }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*  as lxm { IDENT(lxm) }
| eof { raise Eof }
