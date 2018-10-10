%{
  let rec subst_var (name: string) (var: int) (expr: Lang.tml_expr) =
    match expr with
    | EUnit as same-> same
    | EVar (NamedVar name') as same ->
       if name = name' then
         EVar (LambdaVar var)
       else
         same
    | EVar _ as same -> same
    | EVal (VAbs e) -> EVal (VAbs (subst_var name (var + 1) e))
    | EVal (VPair (VAbs e, s)) ->
       EVal (VPair (VAbs (subst_var name (var + 1) e), s))
    | EVal (VPair (f, VAbs e)) ->
       EVal (VPair (f, VAbs (subst_var name (var + 1) e)))
    | EVal _ as same -> same
    | EAppl (s, t) -> EAppl (subst_var name var s, subst_var name var t)
    | EPrimAppl (f, l) ->
       EPrimAppl (f, List.map (fun e -> subst_var name var e) l)
    | EPair (f, s) -> EPair (subst_var name var f, subst_var name var s)
%}
%token LPAREN RPAREN
%token EOL
%token FUN
%token FUNARROW
%token COMMA
%token <int> INT
%token <string> IDENT
%start main             /* the entry point */
%type <Lang.tml_expr> main expr
%type <Lang.tml_val> value
%%
main:
expr EOL { $1 }
;
expr:
  | LPAREN expr RPAREN { $2 }
  | LPAREN RPAREN { EUnit }
  | IDENT { EVar (NamedVar $1) }
  | expr expr { EAppl ($1, $2) }
  | expr COMMA expr { EPair ($1, $3) }
  | value { EVal $1 }
;
value:
  | INT { VInt $1 }
  | FUN IDENT FUNARROW expr { VAbs (subst_var $2 0 $4) }
;
