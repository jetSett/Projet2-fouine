open Expression;;
open SECD;;
open Printf;;
open Lexer;;
open Lexer_type;;

let isAtom = function
  | Variable(_) -> true
  | Const_int(_) -> true
  | Const_bool(_) -> true
  | _ -> false;;

let printExpr channel e =
  let ps s = fprintf channel "%s" s in
  let printVar = function Var(x) -> ps x in
  let rec prE e =
    let isAtomic = isAtom e in
    if not isAtomic
        then ps "(";
    (match e with
    | Unit -> ()
    | Raise(e) -> ps "raise "; prE e;
    | PrInt(e) -> ps "prInt "; prE e;
    | Variable(x) -> printVar x
    | Let_in(x, e1, e2) -> ps "let "; printVar x; ps " = "; prE e1; ps " in "; prE e2
    | Let_rec(x, e1, e2) -> ps "let rec "; printVar x; ps " = "; prE e1; ps " in "; prE e2
    | Let_match(x, y, e1, e2) -> ps "let ("; printVar x; ps ", "; printVar y; ps ") = "; prE e1; ps " in "; prE e2
    | Function_arg(x, e) -> ps "fun "; printVar x; ps " -> "; prE e
    | IfThenElse(c, a, b) -> ps "if "; prE c; ps " then "; prE a; ps " else "; prE b
    | TryWith(e1, Var(x), e2) -> ps "try "; prE e1; ps "with E "; ps x; ps " -> "; prE e2;
    | Const_bool(b) -> if b then ps "true" else ps "false"
    | Not(c) -> ps "not "; prE c
    | And(a, b) -> prE a; ps " && "; prE b
    | Or(a, b) -> prE a; ps " || "; prE b
    | Eq(a, b) -> prE a; ps " = "; prE b
    | Neq(a, b) -> prE a; ps " <> "; prE b
    | Const_int(x) -> print_int x
    | Plus(a, b) -> prE a; ps " + "; prE b
    | Minus(a, b) -> prE a; ps " - "; prE b
    | Times(a, b) -> prE a; ps "*"; prE b
    | Divide(a, b) -> prE a; ps "/"; prE b
    | Apply(a, b) -> prE a; ps " "; prE b
    | Reference(e) -> ps "ref "; prE e
    | Deference(r) -> ps "!"; prE r
    | Imp(a, b) -> prE a; ps "; "; prE b
    | Set(v, b) -> printVar v; ps " := "; prE b
    | Lt(a, b) -> prE a; ps " < "; prE b
    | Gt(a, b) -> prE a; ps " > "; prE b
    | Lte(a, b) -> prE a; ps " <= "; prE b
    | Gte(a, b) -> prE a; ps " >= "; prE b
    | AMake(e) -> ps "aMake "; prE e
    | ArrayAccess(varTab, e2) -> printVar varTab; ps ".("; prE e2; ps ")"
    | ArrayWrite(varTab, e1, e2) -> printVar varTab; ps ".("; prE e1; ps ") <- "; prE e2
    | Comma(a, b) -> prE a; ps ", "; prE b
    );
    if not isAtomic
        then ps ")"
in prE e; ps "\n";;

let printSECD channel e =
  let ps s = fprintf channel "%s" s in
  let printVar = function Var(x) -> ps x in
  let i = ref 0 in
  let print_ind () =
    ps "\n";
    for j=1 to !i do
      ps "  ";
    done
  in
  let ind () = incr i; print_ind () in
  let dnd () = decr i; print_ind () in
  let rec prS = function
    | [] -> ()
    | [x] -> printInstruction x
    | x::xs -> printInstruction x; ps ";"; prS xs
  and printInstruction = function
    (* ARITHMETIC *)
    | ADD -> ps "ADD"
    | SUB -> ps "SUB"
    | MUL -> ps "MUL"
    | DIV -> ps "DIV"
    | CONST(i) -> ps "CONST("; fprintf channel "%d" i; ps ")"
    (* BOOLEANS *)
    | AND -> ps "AND"
    | OR -> ps "OR"
    | NOT -> ps "NOT"
    | EQ -> ps "EQ"
    | NEQ -> ps "NEQ"
    | LT -> ps "LT"
    | GT -> ps "GT"
    | LTE -> ps "LTE"
    | GTE -> ps "GTE"
    (* OTHER *)
    | LET(x) -> print_ind (); ps "LET("; printVar x; ps ")"; ind ()
    | LET_REC(x) -> print_ind (); ps "LET_REC("; printVar x; ps ")"; ind ()
    | ACCESS(x) -> ps "ACCESS("; printVar x; ps ")"
    | CLOS(x, p) -> ps "CLOS("; printVar x; ps ", "; ind (); prS p; ps ")"
    | ENDLET(x) -> dnd (); ps "ENDLET("; printVar x; ps ")"; print_ind ()
    | APPLY -> ps "APPLY"
    | RET -> ps "RET"; dnd ()
    | IF_THEN_ELSE(a, b) -> ps "IF_THEN("; ind (); prS a; dnd (); ps", ELSE("; ind (); prS b; dnd (); ps ")"
    | PR_INT -> ps "PR_INT"
    | RAISE -> ps "RAISE"
    | TRYWITH(Var(x), e2) -> ps "TRYWITH("; ind (); ps x; dnd (); ps ", "; ind (); prS e2; dnd (); ps ")"
    | REF -> ps "REF"
    | DEREF -> ps "DEREF"
    | SET(Var(x)) -> ps "SET("; ps x; ps ")"
    | ARRAY_SET(v) -> ps "ARRAY_SET("; printVar v; ps ")"
    | ARRAY_MAKE -> ps "ARRAY_MAKE"
    | ARRAY_ACCESS(v) -> ps "ARRAY_ACCESS("; printVar v; ps ")"
  in prS e; ps "\n";;

let isAtom_t = function
  | T_Variable(_) -> true
  | T_Const_int(_) -> true
  | T_Const_bool(_) -> true
  | _ -> false;;

let print_fouine_type t =
  let rec aux = function
  | Nothing_t ->"nothing"
  | Int_t -> "int"
  | Ref_t(r) -> "("^(aux r)^" ref)"
  | Tab_t(r) -> "("^(aux r)^" vect)"
  | Pair_t(a, b) -> "("^(aux a)^") * ("^(aux b)^")"
  | Funct_t(t1, t2) -> "("^(aux t1)^" -> "^(aux t2)^")"
  in print_string @@ aux t
;;

let print_typed_expr channel e =
  let ps s = fprintf channel "%s" s in
  let printVar = function Var(x) -> ps x in
  let rec prE e =
    let isAtomic = isAtom_t e in
    if not isAtomic
        then ps "(";
    (match e with
    | T_Unit -> ()
    | T_Raise(e) -> ps "raise "; prE e;
    | T_PrInt(e) -> ps "prInt "; prE e;
    | T_Variable(x) -> printVar x
    | T_Let_in(x, t, e1, e2) ->
      ps "let "; printVar x;
      ps " : "; print_fouine_type t;
      ps " = "; prE e1; ps " in "; prE e2
    | T_Let_rec(x, t, e1, e2) ->
      ps "let rec "; printVar x;
      ps " : "; print_fouine_type t;
      ps " = "; prE e1; ps " in "; prE e2
    | T_Let_match(x, t1, y, t2, e1, e2) -> ps "let ("; printVar x; ps ", "; printVar y; ps ") = "; prE e1; ps " in "; prE e2
    | T_Function_arg(x, t1, e, t2) ->
      print_fouine_type t1; ps " -> "; print_fouine_type t2; ps " = ";
      ps "fun "; printVar x; ps " -> "; prE e
    | T_IfThenElse(c, a, b) -> ps "if "; prE c; ps " then "; prE a; ps " else "; prE b
    | T_TryWith(e1, Var(x), e2) -> ps "try "; prE e1; ps "with E "; ps x; ps " -> "; prE e2;
    | T_Const_bool(b) -> if b then ps "true" else ps "false"
    | T_Not(c) -> ps "not "; prE c
    | T_And(a, b) -> prE a; ps " && "; prE b
    | T_Or(a, b) -> prE a; ps " || "; prE b
    | T_Eq(a, b) -> prE a; ps " = "; prE b
    | T_Neq(a, b) -> prE a; ps " <> "; prE b
    | T_Const_int(x) -> print_int x
    | T_Plus(a, b) -> prE a; ps " + "; prE b
    | T_Minus(a, b) -> prE a; ps " - "; prE b
    | T_Times(a, b) -> prE a; ps "*"; prE b
    | T_Divide(a, b) -> prE a; ps "/"; prE b
    | T_Apply(a, b) -> prE a; ps " "; prE b
    | T_Reference(e) -> ps "ref "; prE e
    | T_Deference(r) -> ps "!"; prE r
    | T_Imp(a, b) -> prE a; ps "; "; prE b
    | T_Set(v, b) -> printVar v; ps " := "; prE b
    | T_Lt(a, b) -> prE a; ps " < "; prE b
    | T_Gt(a, b) -> prE a; ps " > "; prE b
    | T_Lte(a, b) -> prE a; ps " <= "; prE b
    | T_Gte(a, b) -> prE a; ps " >= "; prE b
    | T_AMake(e) -> ps "aMake "; prE e
    | T_ArrayAccess(varTab, e2) -> printVar varTab; ps ".("; prE e2; ps ")"
    | T_ArrayWrite(varTab, e1, e2) -> printVar varTab; ps ".("; prE e1; ps ") <- "; prE e2
    | T_Comma(a, b) -> prE a; ps ", "; prE b
    );
    if not isAtomic
        then ps ")"
  in prE e; ps "\n";;
