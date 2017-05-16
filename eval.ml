open Expression;;
open Environment;;
open Dictpush_hashTbl;;
open PrintExpr;;

exception Not_A_Closure;;
exception Not_An_Int;;
exception Not_A_Reference;;
exception Not_An_Array;;
exception Not_A_Couple;;
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

let rec eval_cont k kE env = function
  | Unit -> k @@ Env.Int(0)
  | Variable(x) -> let v = Env.search env x in k v
  | Let_in(x, expr_x, b) ->
    eval_cont (
      fun val_x ->
        Env.push env x val_x; (*Ajouter e l'environnement*)
        eval_cont (fun return -> Env.pop env x; k return) kE env b (*Evaluer le contexte*)
    ) kE env expr_x
  | Let_match(x, y, expr_xy, b) -> (*Comme avant, mais avec 2 identificateurs*)
    eval_cont (
      function
      | Env.Couple(val_x, val_y) ->
        Env.push env x val_x;
        Env.push env y val_y;
        eval_cont (
          fun return ->
            Env.pop env x;
            Env.pop env y;
            k return
        ) kE env b
      | _ -> raise Not_A_Couple
    ) kE env expr_xy
  | Let_rec(f, expr_f, b) ->
    let naive = free_variable_list expr_f in
    let vars = List.filter (fun v -> v <> f) naive in (*f is not a free variable*)
    let env_rec = Env.env_free_var env vars in (*Recuperer les variables libres pour garder la partie de l'environnement interessante*)
    let closure = Env.Closure(expr_f, env_rec) in (*create closure c with env_rec*)
    Env.push env_rec f closure; (*push c into env_rec*)
    Env.push env f closure;
    eval_cont (fun return -> Env.pop env f; k return) kE env b
  | PrInt(e) ->
    eval_cont (
      function
      | Env.Int(i) as a -> print_int i; print_string "\n"; k a
      | _ -> raise Not_An_Int
    ) kE env e
  | TryWith(e1, x, e2) ->
    let catch v =
      Env.push env x v;
      eval_cont (
        fun eval_e2 -> Env.pop env x; k eval_e2
      ) kE env e2
    in eval_cont k (catch::kE) env e1
  | Raise(e) ->
    eval_cont (
      fun exception_value ->
        match kE with
        | catch::_ -> catch exception_value
        | [] -> raise Unhandled_exception
    ) kE env e
  | Function_arg(x, e) as f -> k @@ Env.Closure(f, Env.env_free_var env (free_variable_list f))
  | IfThenElse(b, left, right) ->
    eval_cont (
      fun is_left ->
        if is_left <> Env.Int(0)
        then eval_cont k kE env left
        else eval_cont k kE env right
    ) kE env b
  | Const_bool(false) -> k @@ Env.Int(0)
  | Const_bool(true) -> k @@ Env.Int(1)
  | Not(b) -> eval_cont (fun v -> k (if v <> Env.Int(0) then Env.Int(0) else Env.Int(1))) kE env b
  | And(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 &&$ e2)) kE env b) kE env a
  | Or(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 ||$ e2)) kE env b) kE env a
  | Eq(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k @@ cond (e1 = e2)) kE env b) kE env a
  | Neq(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k @@ cond (e1 <> e2)) kE env b) kE env a
  | Lt(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 <$ e2)) kE env b) kE env a
  | Gt(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 >$ e2)) kE env b) kE env a
  | Lte(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 <=$ e2)) kE env b) kE env a
  | Gte(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 >=$ e2)) kE env b) kE env a
  | Const_int(i) -> k @@ Env.Int(i)
  | Plus(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 +$ e2)) kE env b) kE env a
  | Minus(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 -$ e2)) kE env b) kE env a
  | Times(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 *$ e2)) kE env b) kE env a
  | Divide(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k (e1 /$ e2)) kE env b) kE env a
  | Reference(r) -> eval_cont (function i -> k (Env.RefValue(ref i))) kE env r
  | Deference(r) -> eval_cont (function Env.RefValue(i) -> k (!i) | _ -> raise Not_A_Reference) kE env r
  | Imp(a, b) -> eval_cont (fun e1 -> eval_cont k kE env b) kE env a
  | Comma(a, b) -> eval_cont (fun e1 -> eval_cont (fun e2 -> k @@ Env.Couple(e1, e2)) kE env b) kE env a
  | Set(v, b) ->
    eval_cont (
      fun rvalue ->
        let lvalue = match Env.search env v with Env.RefValue(r) -> r | _ -> raise Not_A_Reference in
        lvalue := rvalue; k rvalue
    ) kE env b
  | AMake(e) -> eval_cont (
      function
      | Env.Int(i) -> k @@ Env.Array(Array.make i (Env.Int(0)))
      | _ -> raise Not_An_Int
    ) kE env e
  | ArrayAccess(varTab, eIndex) ->
    let tab = Env.search env varTab in (
      match tab with
      | Env.Array(t) ->
        eval_cont (
          function
          | Env.Int(i) -> k @@ t.(i)
          | _ -> raise Not_An_Int
        ) kE env eIndex
      | _ -> raise Not_An_Array
    )
  | ArrayWrite(varTab, eIndex, eValue) ->
    let tab = (
      match Env.search env varTab with
      | Env.Array(t) -> t
      | _ -> raise Not_An_Array
    ) in
    eval_cont (
      function
      | Env.Int(i) -> eval_cont (fun v -> tab.(i) <- v; k v) kE env eValue
      | _ -> raise Not_An_Int
    ) kE env eIndex
  | Apply(f, arg) ->
    eval_cont (
      fun eval_arg -> (*evalue l'argument*)
        eval_cont (
          function
          | Env.Closure(Function_arg(x, expr), e) ->
            Env.push e x eval_arg; (*remplace le nom du paramtre par l'evaluation*)
            eval_cont (
              fun return -> (*evalue le contenu de la cloture*)
                Env.pop e x; (*retirer le parametre de l'environnement*)
                k return (*renvoyer le resultat*)
            ) kE e expr
          | _-> raise Not_A_Closure
        ) kE env f
    ) kE env arg
;;

let eval = eval_cont (fun e -> e) [];;

(* DEPRECATED *)

(*
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
        Env.push env x val_x; (*Ajouter e l'environnement*)
        let return = eval env b in (*Evaluer le contexte*)
        Env.pop env x; (*Retirer de l'environnement*)
        return
      )
  | Let_match(x, y, expr_xy, b) -> (*Comme avant, mais avec 2 identificateurs*)
    handle env expr_xy (
      function
      | Env.Couple(val_x, val_y) ->
        Env.push env x val_x;
        Env.push env y val_y;
        let return = eval env b in
        Env.pop env x;
        Env.pop env y;
        return
      | _ -> raise Not_A_Couple
    )
  | Let_rec(f, expr_f, b) ->
    let naive = free_variable_list expr_f in
    let vars = List.filter (fun v -> v <> f) naive in (*f is not a free variable*)
    let env_rec = Env.env_free_var env vars in (*Recuperer les variables libres pour garder la partie de l'environnement interessante*)
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
  | Not(b) -> if (eval env b) <> Env.Int(0) then Env.Int(0) else Env.Int(1)
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
  | Reference(r) -> handle env r (function i -> Env.RefValue(ref i))
  | Deference(r) -> handle env r (function Env.RefValue(i) -> !i | _ -> raise Not_A_Reference)
  | Imp(a, b) -> handle env a (fun eval_a -> eval env b)
  | Comma(a, b) -> handle env a (fun eval_a -> handle env b (fun eval_b -> Env.Couple(eval_a, eval_b)))
  | Set(v, b) ->
    handle env b (
      fun eval_b ->
        let rvalue = eval_b in
        let lvalue = match Env.search env v with Env.RefValue(r) -> r | _ -> raise Not_A_Reference in
        lvalue := rvalue;
        rvalue
    )
  | AMake(e) -> handle env e (function Env.Int(i) -> Env.Array(Array.make i (Env.Int(0))) | _ -> raise Not_An_Int)
  | ArrayAccess(varTab, eIndex) ->
    let tab = Env.search env varTab in (
    match tab with
     | Env.Array(t) ->
       handle env eIndex (
         function
         | Env.Int(i) -> t.(i)
         | _ -> raise Not_An_Int
       )
     | _ -> raise Not_An_Array
    )
  | ArrayWrite(varTab, eIndex, eValue) ->
    let tab = (
      match Env.search env varTab with
      | Env.Array(t) -> t
      | _ -> raise Not_An_Array) in

    handle env eIndex (function
        | Env.Int(i) -> handle env eValue (
            function v -> (tab.(i) <- v; v)
          )
        | _ -> raise Not_An_Int
      )
  | Apply(f, arg) ->
    handle env f (
      function
      | Env.Closure(Function_arg(x, expr), e) ->
        handle env arg (
          fun eval_arg -> (*evalue l'argument*)
            Env.push e x eval_arg; (*remplace le nom du paramtre par l'evaluation*)
            let return = eval e expr in (*evalue le contenu de la cloture*)
            Env.pop e x; (*retirer le parametre de l'environnement*)
            return (*renvoyer le resultat*)
        )
      | _-> raise Not_A_Closure
    )
;;*)
