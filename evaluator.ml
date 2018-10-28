open Lang

exception ExecutionError of string

let error str = raise (ExecutionError ("execution error: " ^ str))

let rec string_of_val: tml_val -> string = function
  | VUnit -> "()"
  | VInt i -> string_of_int i
  | VString str -> str
  | VAbs _ -> "<function>"
  | VPair (f, s) -> "(" ^ string_of_val f ^ ", " ^ string_of_val s ^ ")"

type environment_type = (var_type * tml_val) list

let extend_env k v (env: environment_type) = (k, v) :: env

let rec lookup (var: var_type) (env: environment_type): tml_val option =
  match env with
  | [] -> None
  | (variable, value) :: env' -> if variable = var then
                                    Some value
                                  else
                                    lookup var env'

let rec subst_var (expr: Lang.tml_expr) (var: int) (value: tml_val) =
  match expr with
  | EUnit as same-> same
  | EVar (LambdaVar var') as same ->
     if var = var' then EVal value
     else same
  | EVar _ as same -> same
  | EVal (VAbs (t,e)) -> EVal (VAbs (t,subst_var e (var + 1) value))
  | EVal (VPair (VAbs (t, e), s)) ->
     EVal (VPair (VAbs (t, subst_var e (var + 1) value), s))
  | EVal (VPair (f, VAbs (t,e))) ->
     EVal (VPair (f, VAbs (t,subst_var e (var + 1) value)))
  | EVal _ as same -> same
  | EAppl (s, t) -> EAppl (subst_var s var value, subst_var t var value)
  | EPrimAppl (f, l) ->
     EPrimAppl (f, List.map (fun e -> subst_var e var value) l)
  | EPair (f, s) -> EPair (subst_var f var value, subst_var s var value)
  | ELet (v, t, e, c) -> ELet (v, t, subst_var e var value, subst_var c var value)

let rec eval (expr: tml_expr) (env: environment_type): tml_val =
  match expr with
  | EUnit -> VUnit
  | EVar v -> begin
    match lookup v env with
     | Some value -> value
     | None -> error "undefined variable"
    end
  | EVal v -> v
  | EAppl (s, t) -> apply s t env
  | EPrimAppl (op, l) -> apply_operation op l env
  | EPair (f, s) -> VPair (eval f env, eval s env)
  | ELet (var, _, expr', cont) ->
     let ev_expr' = eval expr' env in
     let new_env = extend_env var ev_expr' env in
     eval cont new_env
and apply (f: tml_expr) (arg: tml_expr) (env: environment_type): tml_val =
  match eval f env with
  | VAbs (_, e) -> let argval = eval arg env in
                   eval (subst_var e 0 argval) env
  | _ -> error "bad application"
and apply_operation (op: primitive_op) (l: tml_expr list) (env: environment_type): tml_val =
  let evaled_l = List.map (fun e -> eval e env) l in
  let check_arguments sz f =
    let actual_size = List.length l in
    if actual_size <> sz then error ("unexpected number of arguments to "
                                     ^ (string_of_op f) ^ ": "
                                     ^ string_of_int actual_size
                                     ^ "instead of " ^ string_of_int sz) in
  let _ = check_arguments 2 op in
  match (List.nth evaled_l 0, List.nth evaled_l 1) with
  | (VInt x'), (VInt y') ->
     begin match op with
     | PLUS -> VInt (x' + y')
     | MINUS -> VInt (x' - y')
     | MULT -> VInt (x' * y')
     | DIV -> VInt (x' / y')
     end
  | _ -> error "unexpected input to \"+\""

let initial_env: environment_type =
  (NamedVar "id", (VAbs (None, EVar (LambdaVar 0)))) :: []
