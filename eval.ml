open Expression;;
open Environment;;
open Dictpush_hashTbl;;

exception Not_A_Closure;;
exception Not_An_Int;;
exception Not_A_Reference;;
exception Unhandled_exception;;

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

let exceptionStack = ref [] (* stack of environment*variable*expr for exception handling  *)

let rec handle env a f = (*to interrupt computation when an exception rises*)
  let before = List.length !exceptionStack in
  let eval_a = eval env a in
  let after = List.length !exceptionStack in
  if before = after then f eval_a else eval_a
and eval env = function
  | Unit -> Env.Int(0)
  | Raise(e) -> let result = eval env e in
      (match !exceptionStack with
        | [] -> raise Unhandled_exception
        | (nEnv, x, expr)::q -> exceptionStack := q; Env.push nEnv x result; eval nEnv expr
      )
  | Variable(x) ->
    let v = Env.search env x in v
  | Let_in(x, expr_x, b) ->
    handle env expr_x (fun val_x ->
        Env.push env x val_x;
        let return = eval env b in
        Env.pop env x;
        return
      )
  | Let_rec(f, expr_f, b) ->
    let naive = free_variable_list expr_f in
    let vars = List.filter (fun v -> v <> f) naive in (*f is not a free variable*)
    let env_rec = Env.env_free_var env vars in
    let closure = Env.Closure(expr_f, env_rec) in (*create closure c with env_rec*)
    Env.push env_rec f closure; (*push c into env_rec*)
    Env.push env f closure;
    let return = eval env b in
    Env.pop env f;
    return
  | PrInt(e) ->
    handle env e
      (fun a ->
         match a with
         | Env.Int(i) -> print_int i; print_string "\n"; a
         | _ -> raise Not_An_Int
      )
  | TryWith(e1, x, e2) ->
        (* on copie l'environment actuel, on garde la variable qui sera affectée
           et le code à exécuter si jamais on catch une exception *)
    exceptionStack := (Env.copy env, x, e2)::!(exceptionStack);
    eval env e1
  | Function_arg(x, e) as f -> Env.Closure(f, Env.env_free_var env (free_variable_list f))
  | IfThenElse(b, left, right) ->
    handle env b (fun is_left ->
        if is_left <> Env.Int(0)
        then eval env left else eval env right
      )
  | Const_bool(false) -> Env.Int(0)
  | Const_bool(true) -> Env.Int(1)
  | Not(b) -> eval env b
  | And(a, b) -> handle env a (fun eval_a -> eval_a &&$ (eval env b))
  | Or(a, b) -> handle env a (fun eval_a -> eval_a ||$ (eval env b))
  | Eq(a, b) -> handle env a (fun eval_a -> cond (eval_a = (eval env b)))
  | Neq(a, b) -> handle env a (fun eval_a -> cond (eval_a <> (eval env b)))
  | Lt(a, b) -> handle env a (fun eval_a -> eval_a <$ (eval env b))
  | Gt(a, b) -> handle env a (fun eval_a -> eval_a >$ (eval env b))
  | Lte(a, b) -> handle env a (fun eval_a -> eval_a <=$ (eval env b))
  | Gte(a, b) -> handle env a (fun eval_a -> eval_a >=$ (eval env b))
  | Const_int(i) -> Env.Int(i)
  | Plus(a, b) -> handle env a (fun eval_a -> eval_a +$ (eval env b))
  | Minus(a, b) -> handle env a (fun eval_a -> eval_a -$ (eval env b))
  | Times(a, b) -> handle env a (fun eval_a -> eval_a *$ (eval env b))
  | Divide(a, b) -> handle env a (fun eval_a -> eval_a /$ (eval env b))
  | Reference(r) -> handle env r (function Env.Int(i) -> Env.RefInt(ref i) | _ -> raise Not_An_Int)
  | Deference(r) -> handle env r (function Env.RefInt(i) -> Env.Int(!i) | _ -> raise Not_A_Reference)
  | Imp(a, b) -> handle env a (fun eval_a -> eval env b)
  | Set(v, b) ->
    handle env b (
      fun eval_b ->
        let rvalue = match eval_b with Env.Int(i) -> i | _ -> raise Not_An_Int in
        let lvalue = match Env.search env v with Env.RefInt(r) -> r | _ -> raise Not_A_Reference in
        lvalue := rvalue;
        Env.Int(rvalue)
    )
  | Apply(f, arg) ->
    handle env f (
      function
      | Env.Closure(Function_arg(x, expr), e) ->
        handle env arg (
          fun eval_arg ->
            Env.push e x eval_arg;
            let return = eval e expr in
            Env.pop e x;
            return
        )
      | _-> raise Not_A_Closure
    );;
