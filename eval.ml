open Expression;;
open Environment;;
open Dictpush_hashTbl;;

exception Not_A_Closure;;
exception Not_An_Int;;
exception Not_A_Reference;;

module Env = Environment(Dictpush_hashTbl)

let cond b = if b then Env.Int(1) else Env.Int(0);;

let (&&$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> cond (a <> 0 && b <> 0) | _ -> raise Not_An_Int;;
let (||$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> cond (a <> 0 || b <> 0) | _ -> raise Not_An_Int;;
let ( +$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> Env.Int(a + b) | _ -> raise Not_An_Int;;
let ( -$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> Env.Int(a - b) | _ -> raise Not_An_Int;;
let ( *$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> Env.Int(a * b) | _ -> raise Not_An_Int;;
let ( /$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> Env.Int(a / b) | _ -> raise Not_An_Int;;
let ( <$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> cond (a < b) | _ -> raise Not_An_Int;;
let ( >$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> cond (a > b) | _ -> raise Not_An_Int;;
let (<=$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> cond (a <= b) | _ -> raise Not_An_Int;;
let (>=$ ) a b = match a, b with | Env.Int(a), Env.Int(b) -> cond (a >= b) | _ -> raise Not_An_Int;;

let rec eval env = function
  | Unit -> Env.Int(0)
  | Variable(x) ->
    let v = Env.search env x in v
  | Let_in(x, expr_x, b) ->
    let val_x = eval env expr_x in
    Env.push env x val_x;
    let return = eval env b in
    Env.pop env x;
    return
  | Function_arg(x, e) as f -> Env.Closure(f, Env.env_free_var env (free_variable_list f))
  | IfThenElse(b, left, right) ->
    let is_left = eval env b in
    if is_left <> Env.Int(0) then eval env left else eval env right
  | Const_bool(false) -> Env.Int(0)
  | Const_bool(true) -> Env.Int(1)
  | Not(b) -> eval env b
  | And(a, b) -> (eval env a) &&$ (eval env b)
  | Or(a, b) -> (eval env a) ||$ (eval env b)
  | Eq(a, b) -> cond ((eval env a) = (eval env b))
  | Neq(a, b) -> cond ((eval env a) <> (eval env b))
  | Lt(a, b) -> (eval env a) <$ (eval env b)
  | Gt(a, b) -> (eval env a) >$ (eval env b)
  | Lte(a, b) -> (eval env a) <=$ (eval env b)
  | Gte(a, b) -> (eval env a) >=$ (eval env b)
  | Const_int(i) -> Env.Int(i)
  | Plus(a, b) -> (eval env a) +$ (eval env b)
  | Minus(a, b) -> (eval env a) -$ (eval env b)
  | Times(a, b) -> (eval env a) *$ (eval env b)
  | Divide(a, b) -> (eval env a) /$ (eval env b)
  | Reference(r) -> (match eval env r with Env.Int(i) -> Env.RefInt(i) | _ -> raise Not_An_Int)
  | Deference(r) -> (match eval env r with Env.RefInt(i) -> Env.Int(i) | _ -> raise Not_A_Reference)
  | Imp(a, b) -> let _ = eval env a in eval env b
  | Apply(f, arg) ->
    match eval env f with
    | Env.Closure(Function_arg(x, expr), e) ->
      Env.push e x (eval env arg);
      eval e expr
    | _-> raise Not_A_Closure;;
