open Expression;;
open Environment;;
open Dictpush_hashTbl;;

exception Not_A_Closure;;
exception Not_An_Int;;
exception Not_A_Reference;;
exception Not_An_Array;;
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


let rec handle env a f = (* to interrupt computation when an exception rises *)
  let eval_a = eval env a in
  match eval_a with
    | Env.Raise_except(e) -> eval_a
    | _ -> f eval_a
and eval env = function
  | Unit -> Env.Int(0)
  | Raise(e) -> handle env e (fun result -> Env.Raise_except(result))
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
      let ret = eval env e1 in
      (match ret with (* if we catch an exception *)
        | Env.Raise_except(a) -> Env.push env x a; eval env e2
        | _ -> ret)

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
  | AMake(e) -> handle env e (function Env.Int(i) -> Env.Tab(Array.make i 0) | _ -> raise Not_An_Int)
  | TabAccess(varTab, eIndex) -> let tab = Env.search env varTab in
                                  (match tab with
                                  | Env.Tab(t) -> handle env eIndex (
                                        function Env.Int(i) -> Env.Int(t.(i))
                                                | _ -> raise Not_An_Int
                                    )
                                  | _ -> raise Not_An_Array)

  | TabWrite(varTab, eIndex, eValue) -> let tab = (match Env.search env varTab with
                                                    | Env.Tab(t) -> t
                                                    | _ -> raise Not_An_Array) in
                                  handle env eIndex
                                  (function
                                    | Env.Int(i) -> handle env eValue (
                                      function Env.Int(v) -> (tab.(i) <- v; Env.Int(0))
                                                | _ -> raise Not_An_Int
                                      )
                                    | _ -> raise Not_An_Int
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
