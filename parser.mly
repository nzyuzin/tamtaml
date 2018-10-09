%token LPAREN RPAREN
%token EOL
%token FUN
%token FUNARROW
%token COMMA
%token <int> INT
%token <string> IDENT
%start main             /* the entry point */
%type <Lang.tml_expr> main expr
%%
main:
expr EOL { $1 }
;
expr:
  | LPAREN RPAREN { EUnit }
  | FUN IDENT FUNARROW expr { EVal (VAbs ($2, $4)) }
  | IDENT { EVar $1 }
  | INT { EVal (VInt $1) }
  | LPAREN expr expr RPAREN { EAppl ($2, $3) }
  | LPAREN expr COMMA expr RPAREN { EPair ($2, $4) };
