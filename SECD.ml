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
  | ENDLET of variable
  | APPLY
  | RET
  | IF_THEN_ELSE
  | PR_INT
  | RAISE (* raise the value in the top of the stack *)
  | TRYWITH of variable * secd_program (* the variable and the "with" expression *)
;;

exception Not_Supported_Yet of expr;;

let rec compile = function
  | Unit -> [CONST(0)]
  | PrInt(e) -> (compile e) @ [PR_INT]
  | Variable(x) -> [ACCESS(x)]
  | Let_in(x, e1, e2) -> (compile e1) @ [LET(x)] @ (compile e2) @ [ENDLET(x)]
  | Function_arg(x, e) -> [CLOS(x, (compile e) @ [RET])]
  | IfThenElse(c, a, b) -> (compile b) @ (compile a) @ (compile c) @ [IF_THEN_ELSE] (* on the end, the stack : b, resulta, resultb, s *)
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
  | Imp(a, b) -> (compile a) @ (compile b)
  | Raise(e) -> (compile e) @ [RAISE]
  | TryWith(e1, x, e2) -> TRYWITH(x, compile e2)::(compile e1)
  | _ as e -> raise(Not_Supported_Yet(e))
  (*
  | Let_rec(x, e1, e2) ->
  | Reference(e) ->
  | Deference(r) ->
  | Set(v, b) ->*)
;;
