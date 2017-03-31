type variable = Var of string;;

type expr =
  | Unit
  | Variable of variable
  | Let_in of variable * expr * expr
  | Function_arg of variable * expr
  | IfThenElse of expr * expr * expr
  | Const_bool of bool
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Const_int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Apply of expr * expr (* TODO : parse it *)
;;

let map_fun variables expr =
  let rec aux = function
    | [] -> expr
    | v::xs -> Function_arg(v, aux xs)
  in aux variables;;
