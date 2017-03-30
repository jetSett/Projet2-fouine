open Expression;;

let printVar = function Var(x) -> print_string x;;

let isAtom = function
  | Variable(_) -> true
  | Const_int(_) -> true
  | Const_bool(_) -> true
  | _ -> false;;

let rec printExpr e =
  let ps = print_string in
  let isAtomic = isAtom e in
  if not isAtomic
      then ps "(";
  (match e with
  | Unit -> ()
  | Variable(x) -> printVar x
  | Let_in(x, e1, e2) -> ps "Let "; printVar x; ps " = "; printExpr e1; ps " in "; printExpr e2
  | Function_arg(x, e) -> ps "fun "; printVar x; ps " -> "; printExpr e
  | IfThenElse(c, a, b) -> ps "if "; printExpr c; ps " then "; printExpr a; ps " else "; printExpr b
  | Const_bool(b) -> if b then ps "true" else ps "false"
  | Not(c) -> ps "not "; printExpr c
  | And(a, b) -> printExpr a; ps "and"; printExpr b
  | Or(a, b) -> printExpr a; ps "or"; printExpr b
  | Eq(a, b) -> printExpr a; ps " == "; printExpr b
  | Neq(a, b) -> printExpr a; ps " <> "; printExpr b
  | Const_int(x) -> print_int x
  | Plus(a, b) -> printExpr a; ps " + "; printExpr b
  | Minus(a, b) -> printExpr a; ps " - "; printExpr b
  | Times(a, b) -> printExpr a; ps "*"; printExpr b
  | Divide(a, b) -> printExpr a; ps "/"; printExpr b
  );
  if not isAtomic
      then ps ")";;
