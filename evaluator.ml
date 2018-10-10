open Lang

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

let rec subst_var (expr: Lang.tml_expr) (var: int) (value: tml_val) =
  match expr with
  | EUnit as same-> same
  | EVar (LambdaVar var') as same ->
     if var = var' then EVal value
     else same
  | EVar _ as same -> same
  | EVal (VAbs e) -> EVal (VAbs (subst_var e (var + 1) value))
  | EVal (VPair (VAbs e, s)) ->
     EVal (VPair (VAbs (subst_var e (var + 1) value), s))
  | EVal (VPair (f, VAbs e)) ->
     EVal (VPair (f, VAbs (subst_var e (var + 1) value)))
  | EVal _ as same -> same
  | EAppl (s, t) -> EAppl (subst_var s var value, subst_var t var value)
  | EPrimAppl (f, l) ->
     EPrimAppl (f, List.map (fun e -> subst_var e var value) l)
  | EPair (f, s) -> EPair (subst_var f var value, subst_var s var value)

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
  | EPrimAppl (f, l) -> primitive_apply f l env
  | EPair (f, s) -> VPair (eval f env, eval s env)
and apply (f: tml_expr) (arg: tml_expr) (env: environment_type): tml_val =
  match eval f env with
  | VAbs e -> let argval = eval arg env in
                   eval (subst_var e 0 argval) env
  | _ -> error "bad application"
and primitive_apply (f: ident) (l: tml_expr list) (env: environment_type): tml_val =
  let evaled_l = List.map (fun e -> eval e env) l in
  let check_arguments sz f =
    let actual_size = List.length l in
    if actual_size != sz then error ("unexpected number of arguments to " ^ f
                                     ^ ": " ^ string_of_int actual_size
                                     ^ "instead of " ^ string_of_int sz) in
  match f with
  | "+" as p -> let _ = check_arguments 2 p in
                begin
                  match (List.nth evaled_l 0, List.nth evaled_l 1) with
                  | (VInt x'), (VInt y') -> VInt (x' + y')
                  | _ -> error "unexpected input to \"+\""
                end
  | _ -> error "unkown primitive function"

let initial_env: environment_type =
  (NamedVar "id", (VAbs (EVar (LambdaVar 0))))
  :: (NamedVar "+", (VAbs (EVal (VAbs (EPrimAppl
                                         ("+",
                                          [(EVar (LambdaVar 0));
                                           (EVar (LambdaVar 1))]))))))
  :: []
