open Expression;;

type secd_program = instruction list
and instruction =
  (* ARITHMETIC *)
  | ADD
  | SUB
  | MUL
  | DIV
  | CONST of int
  (* BOOLEANS *)
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  (* OTHER *)
  | LET of variable
  | ACCESS of variable
  | CLOS of variable * secd_program
  | ENDLET
  | APPLY
  | RET
  | IF_THEN_ELSE
  | PR_INT
;;

exception Not_Supported_Yet of expr;;

let rec compile = function
  | Unit -> [CONST(0)]
  | PrInt(e) -> [PR_INT]
  | Variable(x) -> [ACCESS(x)]
  | Let_in(x, e1, e2) -> (compile e1) @ [LET(x)] @ (compile e2) @ [ENDLET]
  | Function_arg(x, e) -> [CLOS(x, (compile e) @ [RET])]
  | IfThenElse(c, a, b) -> (compile b) @ (compile a) @ (compile c) @ [IF_THEN_ELSE]
  | Const_bool(true) -> [CONST(1)]
  | Const_bool(false) -> [CONST(0)]
  | Not(c) -> (compile c) @ [NOT]
  | And(a, b) -> (compile b) @ (compile a) @ [AND]
  | Or(a, b) -> (compile b) @ (compile a) @ [OR]
  | Eq(a, b) -> (compile b) @ (compile a) @ [EQ]
  | Neq(a, b) -> (compile b) @ (compile a) @ [NEQ]
  | Lt(a, b) -> (compile b) @ (compile a) @ [LT]
  | Gt(a, b) -> (compile b) @ (compile a) @ [GT]
  | Lte(a, b) -> (compile b) @ (compile a) @ [LTE]
  | Gte(a, b) -> (compile b) @ (compile a) @ [GTE]
  | Const_int(x) -> [CONST(x)]
  | Plus(a, b) -> (compile b) @ (compile a) @ [ADD]
  | Minus(a, b) -> (compile b) @ (compile a) @ [SUB]
  | Times(a, b) -> (compile b) @ (compile a) @ [MUL]
  | Divide(a, b) -> (compile b) @ (compile a) @ [DIV]
  | Apply(a, b) -> (compile b) @ (compile a) @ [APPLY]
  | _ as e -> raise(Not_Supported_Yet(e))
  (*
  | Let_rec(x, e1, e2) ->
  | TryWith(e1, Var(x), e2) ->
  | Raise(x) ->
  | Reference(e) ->
  | Deference(r) ->
  | Imp(a, b) ->
  | Set(v, b) ->*)
;;
