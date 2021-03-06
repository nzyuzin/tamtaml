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
    | EVal (VAbs (t,e)) -> EVal (VAbs (t,subst_var name (var + 1) e))
    | EVal (VPair (VAbs (t,e), s)) ->
       EVal (VPair (VAbs (t,subst_var name (var + 1) e), s))
    | EVal (VPair (f, VAbs (t,e))) ->
       EVal (VPair (f, VAbs (t,subst_var name (var + 1) e)))
    | EVal _ as same -> same
    | EAppl (s, t) -> EAppl (subst_var name var s, subst_var name var t)
    | EPrimAppl (f, l) ->
       EPrimAppl (f, List.map (fun e -> subst_var name var e) l)
    | EPair (f, s) -> EPair (subst_var name var f, subst_var name var s)
    | ELet (v, t, e, c) ->
       if v = NamedVar name then
         ELet (v, t, subst_var name var e, c)
       else
         ELet (v, t, subst_var name var e, subst_var name var c)
%}
%token LET FUN IN EQUALS FUNARROW COMMA LPAREN RPAREN
%token PLUS MINUS
%token MULT DIV
%token <int> INT
%token <string> STRING
%token <string> IDENT
%token ENDEXPR
%token EOF
%start main             /* the entry point */
%type <Lang.tml_expr> main expr arithexpr
%type <Lang.tml_val> value

%nonassoc letexp
%right FUNC
%nonassoc FUN LET LPAREN STRING INT IDENT
%left COMMA
%left PLUS MINUS
%left MULT DIV
%nonassoc APPL

%%
exprend:
  | ENDEXPR {}
  | EOF {}
;
main:
  | EOF { raise Util.Eof }
  | expr exprend { $1 }
;
expr:
  | LPAREN expr RPAREN { $2 }
  | LPAREN RPAREN { EUnit }
  | IDENT { EVar (NamedVar $1) }
  | expr expr %prec APPL { EAppl ($1, $2) }
  | expr COMMA expr { EPair ($1, $3) }
  | LET IDENT EQUALS expr IN expr %prec letexp
    { ELet (NamedVar $2, None, $4, $6) }
  | arithexpr { $1 }
  | value { EVal $1 }
;
arithexpr:
  | expr PLUS expr { EPrimAppl (Lang.PLUS, [$1; $3]) }
  | expr MINUS expr { EPrimAppl (Lang.MINUS, [$1; $3]) }
  | expr DIV expr { EPrimAppl (Lang.DIV, [$1; $3]) }
  | expr MULT expr { EPrimAppl (Lang.MULT, [$1; $3]) }
;
value:
  | INT { VInt $1 }
  | STRING { VString $1 }
  | FUN IDENT FUNARROW expr %prec FUNC { VAbs (None, subst_var $2 0 $4) }
;
