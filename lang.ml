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

type context_type = (var_type * tml_val) list

let rec lookup (var: var_type) (ctxt: context_type): tml_val option =
  match ctxt with
  | [] -> None
  | (variable, value) :: ctxt' -> if variable = var then
                                    Some value
                                  else
                                    lookup var ctxt'

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

let rec eval (expr: tml_expr) (ctxt: context_type): tml_val =
  match expr with
  | EUnit -> VUnit
  | EVar v -> begin
     match lookup v ctxt with
     | Some value -> value
     | None -> VUnit
    end
  | EVal v -> v
  | EAppl (s, t) -> apply s t ctxt
  | EPair (f, s) -> VPair (eval f ctxt, eval s ctxt)
and apply (f: tml_expr) (arg: tml_expr) (ctxt: context_type): tml_val =
  match eval f ctxt with
  | VAbs (v, e) -> let argval = eval arg ctxt in
                   eval (subst_expr e v argval) ctxt
  | _ -> raise (ExecutionError "Bad application")

let initial_context = ("id", (VAbs ("x", EVar "x"))) :: []

let () =
  let expr = EAppl (EVar "id", EVal (VInt 5)) in
  let ev = eval expr initial_context in
  print_endline (string_of_val ev)
