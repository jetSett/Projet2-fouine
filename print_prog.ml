open Program;;

open Printf;;

let rec
print_program prog = match prog with
  | Nil -> printf "()";
  | IfThenElse(expr, t, e) -> printf "if ";
                              print_bool_expr expr;
                              printf "\nthen\n";
                              print_program t;
                              printf "else\n";
                              print_program e;
  | Let_x_prog_in(Var(x), p, inprog) -> printf "let %s = " x;
                                        print_program p;
                                        printf " in\n";
                                        print_program inprog;
  | Let_x_arith_expr_in(Var(x), p, inprog) -> printf "let %s = " x;
                                        print_arith_expr p;
                                        printf " in\n";
                                        print_program inprog;

  | Let_x_boolexpr_in(Var(x), b, inprog) -> printf "let %s = " x;
                                        print_bool_expr b;
                                        printf " in\n";
                                        print_program inprog;
  | Let_x_function_in(Var(x), f, inprog) -> printf "let %s = " x;
                                        print_function f;
                                        printf " in\n";
                                        print_program inprog;

  | Eval(ev) ->  printf "Eval (";
                              print_evaluation ev; printf ")";
and
print_bool_expr bexpr = match bexpr with
  | Const_bool(b) -> if b then printf "true" else printf "false"
  | Not(b) -> printf "not ("; print_bool_expr b; printf ")";
  | And(b1, b2) -> printf "("; print_bool_expr b1; printf ") && (";
                   print_bool_expr b2; printf ")";
  | Or(b1, b2) -> printf "("; print_bool_expr b1; printf ") || (";
                print_bool_expr b2; printf ")";
  | Eval_bool(e) -> printf "Eval_bool("; print_evaluation e;
                         printf ")";

and

print_arith_expr aexpr = match aexpr with
  | Const_int(i) -> printf "Int(%d)" i;
  | Plus(a, b) -> printf "("; print_arith_expr a; printf " + ";
                  print_arith_expr b; printf ")";
  | Times(a, b) -> printf "("; print_arith_expr a; printf " * ";
                  print_arith_expr b; printf ")";
  | Minus(a, b) -> printf "("; print_arith_expr a; printf " - ";
                  print_arith_expr b; printf ")";
  | Divide(a, b) -> printf "("; print_arith_expr a; printf " / ";
                  print_arith_expr b; printf ")";
  | Eval_arith(e) -> printf "Eval_arith("; print_evaluation e;
                         printf ")";

and

print_evaluation eval = match eval with
  | Eval_function_arg(ev, p) -> printf "Eval_fun("; print_evaluation ev; printf ", ";
                                print_evaluation p; printf ")";
  | Eval_function_var(Var(x), p) -> printf "Eval_fun(%s, " x; print_evaluation p;
                                    printf ")";
  | Eval_function_anon(f, p) -> printf "Eval_fun("; print_function f; printf ", ";
                                print_evaluation p; printf ")";
  | Eval_arith_expr(aexpr) -> print_arith_expr aexpr;
  | Eval_bool_expr(bexpr) -> print_bool_expr bexpr;
  | Eval_var(Var(v)) -> printf "Var (%s)" v
  | Eval_prog(p) -> printf "Eval_prog ("; print_program p; printf ")";

and
 print_function funct = match funct with
  | Function_arg(Var(x), p) -> printf "fun %s -> (" x; print_program p; printf ")"
  | Function_noArg(p) -> printf "fun () -> ("; print_program p; printf ")"
