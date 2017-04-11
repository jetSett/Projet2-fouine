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

let rec print_list = function
  | Var(x)::q -> print_string x; print_string "\n"; print_list q;
  | [] -> print_string "\n"

let free_variable_list prog =
  let rec merge l1 = function
    | [] -> l1
    | x::q when List.mem x l1 -> merge l1 q
    | x::q -> merge (x::l1) q
  in
  let rec aux prog linked =
    match prog with
      | [] -> []

      | SET(v)::q when not (List.mem v linked) -> v::(aux q (linked))
      | SET(v)::q -> aux q linked

      | IF_THEN_ELSE(p1, p2)::q -> merge (aux q linked) (merge (aux p1 linked) (aux p2 linked))

      | LET(v)::q -> aux q (if List.mem v linked then linked else (v::linked))
      | LET_REC(v)::q -> aux q (if List.mem v linked then linked else (v::linked))


      | ENDLET(v)::q -> let b = ref true in
                        let nLinked = List.filter (fun a -> if !b then (if a<>v then (b:= false; false) else true) else true ) linked in (* remove the first occurence of v*)
                        aux q nLinked

      | ACCESS(v)::q when not (List.mem v linked) -> v::(aux q linked)
      | ACCESS(v)::q -> aux q linked

      | CLOS(v, p)::q -> merge (aux p (v::linked)) (aux q linked)

      | TRYWITH(v, p)::q -> merge (aux p linked) (aux q (v::linked))
      | _::q -> aux q linked in
    aux prog []

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
