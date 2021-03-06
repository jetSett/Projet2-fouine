type variable = Var of string;;

type expr =
  | Unit
  | Const_int of int
  | Const_bool of bool
  | Variable of variable
  | Let_in of variable * expr * expr
  | Let_rec of variable * expr * expr
  | Let_match of variable * variable * expr * expr
  | Function_arg of variable * expr
  | Not of expr
  | Raise of expr
  | IfThenElse of expr * expr * expr
  | TryWith of expr * variable * expr
  | AMake of expr
  | ArrayAccess of variable * expr
  | ArrayWrite of variable * expr * expr (* var.( e1 ) <- e2 *)
  | PrInt of expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Apply of expr * expr
  | Reference of expr
  | Deference of expr
  | Imp of expr * expr
  | Set of variable * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Lte of expr * expr
  | Gte of expr * expr
  | Comma of expr * expr
;;

let free_variable_list e =
  let rec merge l1 = function
    | [] -> l1
    | x::q when List.mem x l1 -> merge l1 q
    | x::q -> merge (x::l1) q
  in
  let rec aux linked_v = function
    | Unit | Const_int(_) | Const_bool(_) -> []
    | Variable(v) -> if List.mem v linked_v then [] else [v]
    | Let_rec(x, e1, e2) ->
      let l =
        if List.mem x linked_v
        then linked_v else x::linked_v
      in merge (aux l e1) (aux l e2)
    | Let_in(x, e1, e2) ->
      let l1 =
        if List.mem x linked_v
          then aux linked_v e2
          else aux (x::linked_v) e2
      in merge l1 (aux linked_v e1)
    | Let_match(x, y, e1, e2) ->
      let l_x = if List.mem x linked_v then linked_v else x::linked_v in
      let l_xy = if List.mem y l_x then l_x else y::l_x in
      let l = aux l_xy e2 in
      merge l (aux linked_v e1)
    | Function_arg(x, e1) ->
      if List.mem x linked_v
        then aux linked_v e1
        else aux (x::linked_v) e1
    | Not(e1) -> aux linked_v e1
    | Raise(e1) -> aux linked_v e1
    | TryWith(e1, x, e2) -> merge (aux linked_v e1) (aux (x::linked_v) e2)
    | PrInt(e1) -> aux linked_v e1
    | And(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Or(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Eq(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Neq(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Plus(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Minus(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Times(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Divide(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Apply(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2) (* worst case : too much registrations *)
    | IfThenElse(e1, e2, e3) -> merge (aux linked_v e1) (merge (aux linked_v e2) (aux linked_v e3))
    | Reference(e1) -> aux linked_v e1
    | Deference(e1) -> aux linked_v e1
    | Imp(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Comma(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Set(v, e2) -> merge (aux linked_v (Variable(v))) (aux linked_v e2)
    | Lt(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Gt(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Lte(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | Gte(e1, e2) -> merge (aux linked_v e1) (aux linked_v e2)
    | AMake(e) -> aux linked_v e
    | ArrayAccess(v, e) -> if List.mem v linked_v then (aux linked_v e) else
                                v::(aux linked_v e)
    | ArrayWrite(v, e1, e2) -> if List.mem v linked_v then merge (aux linked_v e1) (aux linked_v e2) else
                                v::(merge (aux linked_v e1) (aux linked_v e2))
  in
  aux [] e;;
