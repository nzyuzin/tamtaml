{
open Parser (* The type token is defined in parser.mli *)
}
rule token = parse
| [' ' '\t' '\n'] { token lexbuf } (* skip blanks *)
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
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*  as lxm { IDENT(lxm) }
| ";;" { ENDEXPR }
| eof { EOF }
