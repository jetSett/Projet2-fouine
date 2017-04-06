open Expression;;
open Environment;;
open Dictpush_hashTbl;;

exception Not_A_Closure;;
exception Not_An_Int;;
exception Not_A_Reference;;
exception Except_in_eval of int;;

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
  | Raise(i) -> raise (Except_in_eval i)
  | Variable(x) ->
    let v = Env.search env x in v
  | Let_in(x, expr_x, b) ->
    let val_x = eval env expr_x in
    Env.push env x val_x;
    let return = eval env b in
    Env.pop env x;
    return
  | Let_rec(f, expr_f, b) ->
    let naive = free_variable_list expr_f in
    let vars = List.filter (fun v -> v <> f) naive in
    let env_rec = Env.env_free_var env vars in
    let closure = Env.Closure(expr_f, env_rec) in
    Env.push env_rec f closure;
    Env.push env f closure;
    let return = eval env b in
    Env.pop env f;
    return
  | PrInt(e) -> let a = eval env e in
                (match a with
                | Env.Int(i) -> print_int i; print_string "\n"; a
                | _ -> raise Not_An_Int)
  | TryWith(e1, x, e2) -> (* on gère tout avec le mécanisme d'exception de caml *)
                          (* l'idée est que on rattrape une exception si elle est lancée *)
                          let ecopy = Env.copy env in
                          (try
                            eval env e1
                           with Except_in_eval(i) -> (* il y a eu une exception *)
                                                    (* on reviens à l'ancien env et on rajoute la variable *)
                                                    Env.push ecopy x (Env.Int(i));
                                                    eval ecopy e2 )

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
  | Reference(r) -> (match eval env r with Env.Int(i) -> Env.RefInt(ref i) | _ -> raise Not_An_Int)
  | Deference(r) -> (match eval env r with Env.RefInt(i) -> Env.Int(!i) | _ -> raise Not_A_Reference)
  | Imp(a, b) -> let _ = eval env a in eval env b
  | Set(v, b) ->
    let rvalue = match eval env b with Env.Int(i) -> i | _ -> raise Not_An_Int in
    let lvalue = match Env.search env v with Env.RefInt(r) -> r | _ -> raise Not_A_Reference in
    lvalue := rvalue;
    Env.Int(rvalue)
  | Apply(f, arg) ->
    match eval env f with
    | Env.Closure(Function_arg(x, expr), e) ->
      Env.push e x (eval env arg);
      let return = eval e expr in
      Env.pop e x;
      return
    | _-> raise Not_A_Closure;;
