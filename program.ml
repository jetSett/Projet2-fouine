type variable = Var of string;;

type expr =
  | Nil
  | Variable of variable
  | Let_in of variable * expr * expr
  | Function_args of variable * expr
  | IfThenElse of bool_expr * expr * expr
  | BoolExpr of bool_expr
  | ArithExpr of arith_expr

and
  bool_expr = (* a boolean expression *)
  | Const_bool of bool
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr
  | NEq of expr * expr


and
  arith_expr = (* an arithmetic expression *)
  | Const_int of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
