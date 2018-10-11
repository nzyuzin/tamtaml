type ident = string
type var_type = NamedVar of string | LambdaVar of int
type const_type = string

type primitive_op = PLUS | MINUS | MULT | DIV

type tml_type =
  | TUnit
  | TFun of tml_type * tml_type
  | TProd of tml_type * tml_type
  | TType of ident

type tml_expr =
  | EUnit
  | EVar of var_type
  | EVal of tml_val
  | EAppl of tml_expr * tml_expr
  | EPrimAppl of primitive_op * (tml_expr list)
  | EPair of tml_expr * tml_expr
  | ELet of var_type * tml_type option * tml_expr * tml_expr
and tml_val =
  | VUnit
  | VInt of int
  | VString of string
  | VAbs of tml_type option * tml_expr
  | VPair of tml_val * tml_val

let string_of_op = function
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | DIV -> "/"
