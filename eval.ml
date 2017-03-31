open Expression;;
open Environment;;

exception Not_A_Closure;;
exception Not_An_Int;;

let cond b = if b then Int(1) else Int(0);;

let (&&$ ) a b = match a, b with | Int(a), Int(b) -> cond (a <> 0 && b <> 0) | _ -> raise Not_An_Int;;
let (||$ ) a b = match a, b with | Int(a), Int(b) -> cond (a <> 0 || b <> 0) | _ -> raise Not_An_Int;;
let ( +$ ) a b = match a, b with | Int(a), Int(b) -> Int(a + b) | _ -> raise Not_An_Int;;
let ( -$ ) a b = match a, b with | Int(a), Int(b) -> Int(a - b) | _ -> raise Not_An_Int;;
let ( *$ ) a b = match a, b with | Int(a), Int(b) -> Int(a * b) | _ -> raise Not_An_Int;;
let ( /$ ) a b = match a, b with | Int(a), Int(b) -> Int(a / b) | _ -> raise Not_An_Int;;

let rec eval env = function
  | Unit -> Int(0)
  | Variable(x) ->
    (* let v = Env.search env x in v *) Int(-1)
  | Let_in(x, expr_x, b) ->
    let val_x = eval env expr_x in
    (* Env.push env x val_x; *)
    let return = eval env b in
    (* Env.pop env x; *)
    return
  | Function_arg(x, e) as f -> Closure(f, (* copy of *) env)
  | IfThenElse(b, left, right) ->
    let is_left = eval env b in
    if is_left <> Int(0) then eval env left else eval env right
  | Const_bool(false) -> Int(0)
  | Const_bool(true) -> Int(1)
  | Not(b) -> eval env b
  | And(a, b) -> (eval env a) &&$ (eval env b)
  | Or(a, b) -> (eval env a) ||$ (eval env b)
  | Eq(a, b) -> cond ((eval env a) = (eval env b))
  | Neq(a, b) -> cond ((eval env a) <> (eval env b))
  | Const_int(i) -> Int(i)
  | Plus(a, b) -> (eval env a) +$ (eval env b)
  | Minus(a, b) -> (eval env a) -$ (eval env b)
  | Times(a, b) -> (eval env a) *$ (eval env b)
  | Divide(a, b) -> (eval env a) /$ (eval env b)
  | Apply(f, arg) ->
    match eval env f with
    | Closure(Function_arg(x, expr), e') ->
      (* Env.push e' x (eval env arg); *)
      eval e' expr
    | _-> raise Not_A_Closure;;
