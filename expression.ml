type variable = Var of string;;

type expr =
  | Unit
  | Const_int of int
  | Const_bool of bool
  | Variable of variable
  | Let_in of variable * expr * expr
  | Function_arg of variable * expr
  | Not of expr
  | IfThenElse of expr * expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Apply of expr * expr
;;

let map_fun variables expr =
  let rec aux = function
    | [] -> expr
    | v::xs -> Function_arg(v, aux xs)
  in aux variables;;

let free_variable_list e =
  let rec merge l1 l2 = match l2 with
    | [] -> l1
    | x::q when List.mem x l1 -> merge l1 q
    | x::q -> merge (x::l1) q
  in
  let rec aux e linked_v = match e with
    | Unit | Const_int(_) | Const_bool(_) -> []
    | Variable(v) -> if List.mem v linked_v then [] else [v]
    | Let_in(x, e1, e2) ->let l1 =
                        if List.mem x linked_v then
                          aux e2 linked_v
                        else
                          aux e2 (x::linked_v)
                          in
                          merge l1 (aux e2 linked_v)
    | Function_arg(x, e1) ->
                        if List.mem x linked_v then
                          aux e1 linked_v
                        else
                          aux e1 (x::linked_v)
    | Not(e) -> aux e linked_v
    | And(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Or(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Eq(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Neq(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Plus(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Minus(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Times(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Divide(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v)
    | Apply(e1, e2) -> merge (aux e1 linked_v) (aux e2 linked_v) (* worst case : too much registrations *)
    | IfThenElse(e1, e2, e3) -> merge (aux e1 linked_v) (merge (aux e1 linked_v) (aux e2 linked_v))

  in
  aux e [];;
