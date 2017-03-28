type variable = Var of string;;

type
  prog = (* un programme : les briques de bases *)
  | Nil
  | IfThenElse of bool_expr * prog * prog
  | Let_x_prog_in of variable * prog * prog
  | Let_x_arith_expr_in of variable * arith_expr * prog
  | Let_x_boolexpr_in of variable * bool_expr * prog
  | Let_x_function_in of variable * funct * prog
  | Eval of evaluation

and funct =
  | Function_arg of variable * prog (* fun var -> prog(var) *)
  | Function_noArg of prog

and evaluation = (* l'evaluation de quoi que ce soit *)
  | Eval_function_arg of evaluation * evaluation (* on évalue sur une fonction qui va nous etre donnée *)
  | Eval_function_var of variable * evaluation (* on évalue une fonction dans une variable *)
  | Eval_function_anon of funct * evaluation (* on évalue une fonction anonyme *)
  | Eval_arith_expr of arith_expr
  | Eval_bool_expr of bool_expr
  | Eval_var of variable
  | Eval_prog of prog

and
  bool_expr =
  | Const_bool of bool
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Eval_bool of evaluation

and
  arith_expr =
  | Const_int of int
  | Plus of arith_expr * arith_expr
  | Minus of arith_expr * arith_expr
  | Times of arith_expr * arith_expr
  | Divide of arith_expr * arith_expr
  | Eval_arith of evaluation
