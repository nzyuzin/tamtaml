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

let rec subst_expr (expr: tml_expr) (var: var_type) (sub: tml_val): tml_expr =
  match expr with
  | EUnit -> EUnit
  | EVar var' as same -> if var' = var then EVal sub else same
  | EVal v -> EVal (subst_val v var sub)
  | EAppl (s, t) -> EAppl (subst_expr s var sub, subst_expr t var sub)
  | EPrimAppl (f, l) -> EPrimAppl (f, List.map (fun e -> subst_expr e var sub) l)
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
  | EPrimAppl (f, l) -> primitive_apply f l env
  | EPair (f, s) -> VPair (eval f env, eval s env)
and apply (f: tml_expr) (arg: tml_expr) (env: environment_type): tml_val =
  match eval f env with
  | VAbs (v, e) -> let argval = eval arg env in
                   eval (subst_expr e v argval) env
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

let initial_env = ("id", (VAbs ("x", EVar "x")))
                  :: ("+", (VAbs ("x", EVal (VAbs ("y", EPrimAppl
                                                          ("+",
                                                           [(EVar "x");
                                                            (EVar "y")]))))))
                  :: []
