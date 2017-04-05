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
  | Raise(x) -> ps "raise "; print_int x;
  | PrInt(e) -> ps "prInt "; printExpr e;
  | Variable(x) -> printVar x
  | Let_in(x, e1, e2) -> ps "let "; printVar x; ps " = "; printExpr e1; ps " in "; printExpr e2
  | Let_rec(x, e1, e2) -> ps "let rec "; printVar x; ps " = "; printExpr e1; ps " in "; printExpr e2
  | Function_arg(x, e) -> ps "fun "; printVar x; ps " -> "; printExpr e
  | IfThenElse(c, a, b) -> ps "if "; printExpr c; ps " then "; printExpr a; ps " else "; printExpr b
  | TryWith(e1, Var(x), e2) -> ps "try "; printExpr e1; ps "with E "; ps x; ps " -> "; printExpr e2;
  | Const_bool(b) -> if b then ps "true" else ps "false"
  | Not(c) -> ps "not "; printExpr c
  | And(a, b) -> printExpr a; ps "&&"; printExpr b
  | Or(a, b) -> printExpr a; ps "||"; printExpr b
  | Eq(a, b) -> printExpr a; ps " = "; printExpr b
  | Neq(a, b) -> printExpr a; ps " <> "; printExpr b
  | Const_int(x) -> print_int x
  | Plus(a, b) -> printExpr a; ps " + "; printExpr b
  | Minus(a, b) -> printExpr a; ps " - "; printExpr b
  | Times(a, b) -> printExpr a; ps "*"; printExpr b
  | Divide(a, b) -> printExpr a; ps "/"; printExpr b
  | Apply(a, b) -> printExpr a; ps " "; printExpr b
  | Reference(e) -> print_string "ref "; printExpr e
  | Deference(r) -> print_string "!"; printExpr r
  | Imp(a, b) -> printExpr a; print_string ";"; printExpr b
  | Lt(a, b) -> printExpr a; print_string "<"; printExpr b
  | Gt(a, b) -> printExpr a; print_string ">"; printExpr b
  | Lte(a, b) -> printExpr a; print_string "<="; printExpr b
  | Gte(a, b) -> printExpr a; print_string ">="; printExpr b
  );
  if not isAtomic
      then ps ")";;
