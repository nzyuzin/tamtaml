exception ExecutionError of string

type ident = string
type var_type = ident
type const_type = string

type tml_type =
  | TUnit
  | TFun of var_type * tml_type
  | TProd of tml_type * tml_type
  | TType of ident

type tml_expr =
  | EUnit
  | EVar of var_type
  | EVal of tml_val
  | EAppl of tml_expr * tml_expr
  | EPair of tml_expr * tml_expr
and tml_val =
  | VUnit
  | VInt of int
  | VString of string
  | VAbs of var_type * tml_expr
  | VPair of tml_val * tml_val

let rec string_of_val: tml_val -> string = function
  | VUnit -> "()"
  | VInt i -> string_of_int i
  | VString str -> str
  | VAbs _ -> "<function>"
  | VPair (f, s) -> "(" ^ string_of_val f ^ ", " ^ string_of_val s ^ ")"

type environment_type = (var_type * tml_val) list

let rec lookup (var: var_type) (env: environment_type): tml_val option =
  match env with
  | [] -> None
  | (variable, value) :: env' -> if variable = var then
                                    Some value
                                  else
                                    lookup var env'

let rec subst_expr (expr: tml_expr) (var: var_type) (sub: tml_val): tml_expr =
  match expr with
  | EUnit -> EUnit
  | EVar var' as same -> if var' = var then EVal sub else same
  | EVal v -> EVal (subst_val v var sub)
  | EAppl (s, t) -> EAppl (subst_expr s var sub, subst_expr t var sub)
  | EPair (f, s) -> EPair (subst_expr f var sub, subst_expr s var sub)
and subst_val (value: tml_val) (var: var_type) (sub: tml_val): tml_val =
  match value with
  | VAbs (v, e) -> VAbs (v, subst_expr e var sub)
  | VPair (f, s) -> VPair (subst_val f var sub, subst_val s var sub)
  | _ as same -> same

let rec eval (expr: tml_expr) (env: environment_type): tml_val =
  match expr with
  | EUnit -> VUnit
  | EVar v -> begin
    match lookup v env with
     | Some value -> value
     | None -> VUnit
    end
  | EVal v -> v
  | EAppl (s, t) -> apply s t env
  | EPair (f, s) -> VPair (eval f env, eval s env)
and apply (f: tml_expr) (arg: tml_expr) (env: environment_type): tml_val =
  match eval f env with
  | VAbs (v, e) -> let argval = eval arg env in
                   eval (subst_expr e v argval) env
  | _ -> raise (ExecutionError "Bad application")

let primitive_plus (x: tml_expr) (y: tml_expr): tml_expr =
  match x, y with
  | (EVal (VInt x'), EVal (VInt y')) -> EVal (VInt (x' + y'))
  | _ -> raise (ExecutionError "Unexpected input to \"+\"")

let initial_env = ("id", (VAbs ("x", EVar "x")))
                      :: (("+"), (VAbs ("x", EVal (VAbs ("y",
                                                         primitive_plus
                                                           (EVar "x")
                                                           (EVar "y"))))))
                      :: []

let () =
  let expr = EAppl (EVar "id", (EAppl (EVar "+", EVal (VInt 9)))) in
  let ev = eval expr initial_env in
  print_endline (string_of_val ev)
