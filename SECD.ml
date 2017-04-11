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
  | REF
  | DEREF
  | SET of variable
  | LET of variable
  | LET_REC of variable
  | ACCESS of variable
  | CLOS of variable * secd_program
  | ENDLET of variable
  | APPLY
  | RET
  | IF_THEN_ELSE of secd_program * secd_program
  | PR_INT
  | RAISE (* raise the value in the top of the stack *)
  | TRYWITH of variable * secd_program (* the variable and the "with" expression *)
;;


(* for the moment, all the variables *)
let rec free_variable_list prog =
  match prog with
    | [] -> []
    | SET(v)::q -> v::(free_variable_list q)
    | LET(v)::q -> v::(free_variable_list q)
    | ACCESS(v)::q -> v::(free_variable_list q)
    | CLOS(v, p)::q -> v::(free_variable_list p)@(free_variable_list q)
    | TRYWITH(v, p)::q -> v::(free_variable_list p)@(free_variable_list q)
    | _::q -> free_variable_list q

exception Not_Supported_Yet of expr;;

let rec compile = function
  | Unit -> [CONST(0)]
  | PrInt(e) -> (compile e) @ [PR_INT]
  | Variable(x) -> [ACCESS(x)]
  | Let_in(x, e1, e2) -> (compile e1) @ [LET(x)] @ (compile e2) @ [ENDLET(x)]
  | Let_rec(x, e1, e2) -> (compile e1) @ [LET_REC(x)] @ (compile e2) @ [ENDLET(x)]
  | Function_arg(x, e) -> [CLOS(x, (compile e) @ [RET])]
  | IfThenElse(b, t, e) -> (compile b) @ [IF_THEN_ELSE(compile(t), compile(e))] (* on the end, the stack : b, resulta, resultb, s *)
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
  | Reference(e) -> (compile e) @ [REF]
  | Deference(r) -> (compile r) @ [DEREF]
  | Set(v, b) -> (compile b) @ [SET(v)](* la pile ressemble à ça après : val, ref, s *)
;;
