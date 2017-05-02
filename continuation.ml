open Expression;;

exception Not_Implemented;;

(* upercase because this is forbiden to use upercase starting variables *)
let vEnv = Var("K") and vEnvE = Var("KE")

let indexVar = ref 0
let get_next_variable () = incr indexVar; Var(String.concat "" ["V"; (string_of_int !indexVar)])


let ident = Function_arg(Var("X"), Variable(Var("X")))
let neutral_continuation = ident

let rec replace_vEnv vE vEx e = match e with
| Unit | Const_int(_) | Const_bool(_) -> e
| Variable(v) when v=vEnv-> Variable(vE)
| Variable(v) when v=vEnvE-> Variable(vEx)
| Variable(v) -> e
| Let_in(v, e1, e2) -> Let_in(v, replace_vEnv vE vEx e1, replace_vEnv vE vEx  e2)
| Let_rec(v, e1, e2) -> Let_rec(v, replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Let_match(v1, v2, e1, e2) -> Let_match(v1, v2, replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Function_arg(v, e) -> Function_arg(v, replace_vEnv vE vEx  e)
| Not(e) -> Not(replace_vEnv vE vEx  e)
| IfThenElse(a, b, c) -> IfThenElse(replace_vEnv vE vEx  a, replace_vEnv vE vEx  b, replace_vEnv vE vEx  c)
| PrInt(e) -> PrInt(replace_vEnv vE vEx  e)
| And(e1, e2) -> And(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Or(e1, e2) -> Or(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Eq(e1, e2) -> Eq(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Neq(e1, e2) -> Neq(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Plus(e1, e2) -> Plus(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Minus(e1, e2) -> Minus(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Times(e1, e2) -> Times(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Divide(e1, e2) -> Divide(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Apply(e1, e2) -> Apply(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Lt(e1, e2) -> Lt(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Gt(e1, e2) -> Gt(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Lte(e1, e2) -> Lte(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Gte(e1, e2) -> Gte(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| Comma(e1, e2) -> Comma(replace_vEnv vE vEx  e1, replace_vEnv vE vEx  e2)
| AMake(e) -> AMake(replace_vEnv vE vEx e)
| ArrayAccess(v, e) -> ArrayAccess(v, replace_vEnv vE vEx e)
| ArrayWrite(v, e1, e2) -> ArrayWrite(v, replace_vEnv vE vEx e1, replace_vEnv vE vEx e2)
| _ -> raise Not_Implemented

let transfo e = let vE, vEx = get_next_variable (), get_next_variable () in
                Function_arg(vE, Function_arg(vEx, replace_vEnv vE vEx e))

let transfo_0 e = transfo (Apply(Variable(vEnv), e))

let transfo_1 e vX finalX = transfo (Apply(e,
                                  Function_arg(vX,
                                    Apply(Apply(Variable(vEnv), finalX), Variable vEnvE)
                                  )
                                ))

let transfo_2 e1 e2 vX vY finalXY =
  transfo ( Apply(Apply(e2,
      Function_arg(vY, Apply(Apply(e1,
          Function_arg(vX, Apply(Variable vEnv, finalXY))
        ), Variable vEnvE))
    ),
  Variable vEnvE)
  )

let rec transformation_cont expr = match expr with
  | Unit | Variable(_) | Const_int(_) | Const_bool(_) -> transfo_0 expr
  | Not(e) -> let vX = get_next_variable () in transfo_1 (transformation_cont e) vX (Not(Variable(vX)))
  | Lt(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Lt(Variable(vX), Variable(vY)))
  | Gt(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Gt(Variable(vX), Variable(vY)))
  | Lte(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Lte(Variable(vX), Variable(vY)))
  | Gte(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Gte(Variable(vX), Variable(vY)))
  | And(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (And(Variable(vX), Variable(vY)))
  | Or(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Or(Variable(vX), Variable(vY)))
  | Eq(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Eq(Variable(vX), Variable(vY)))
  | Neq(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Neq(Variable(vX), Variable(vY)))
  | Plus(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Plus(Variable(vX), Variable(vY)))
  | Minus(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Minus(Variable(vX), Variable(vY)))
  | Times(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                    transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Times(Variable(vX), Variable(vY)))
  | Divide(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                      transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Divide(Variable(vX), Variable(vY)))

  | Comma(e1, e2) -> let vX, vY = get_next_variable (), get_next_variable () in
                      transfo_2 (transformation_cont e1) (transformation_cont e2) vX vY (Comma(Variable(vX), Variable(vY)))

  | IfThenElse(b, e1, e2) -> let vX = get_next_variable () in
    transfo (Apply( Apply( transformation_cont b,
        Function_arg(vX, IfThenElse(Variable(vX), Apply ( Apply (transformation_cont e1, Variable(vEnv)), Variable vEnvE),
                                                  Apply ( Apply (transformation_cont e2, Variable vEnv), Variable vEnvE)))
       ) ,Variable vEnvE))

  | Let_in(var, e1, e2) -> transfo
                              (Apply( Apply (transformation_cont e1,
                                Function_arg(var,
                                  Apply(Apply(transformation_cont e2, Variable(vEnv)), Variable vEnvE)
                                )), Variable vEnvE)
                            )

  | Function_arg(x, e) -> transfo (Apply (Variable(vEnv), Function_arg(x, transformation_cont e)))

  | Apply(f, e) -> let vX, vF = get_next_variable (), get_next_variable () in
                              transfo (Apply (Apply(transformation_cont e,
                                Function_arg(vX,
                                Apply (Apply(transformation_cont f, Function_arg(vF,
                                  Apply(Apply(Apply(Variable(vF), Variable(vX)), Variable(vEnv)), Variable vEnvE)
                                )), Variable vEnvE)
                              )), Variable vEnvE))

  | PrInt(e) -> let vX = get_next_variable () in transfo (Apply(Apply(transformation_cont e,
                          Function_arg(vX, Apply(Variable(vEnv), PrInt(Variable(vX))))), Variable vEnvE))

  | TryWith(e1, var, e2) ->
                        transfo (Apply (Apply(transformation_cont e1, Variable vEnv),
                        Function_arg(var, Apply (Apply(transformation_cont e2, Variable vEnv), Variable vEnvE)
                          )))
  | Raise(e) -> transfo (Apply(Apply(transformation_cont e, Variable vEnvE), Variable vEnvE))

  | ArrayAccess(var, e) -> let vX = get_next_variable () in
            transfo (Apply(Apply(transformation_cont e, Function_arg(vX, Apply(Variable vEnv, ArrayAccess(var, Variable vX)) )), Variable vEnvE))

  | ArrayWrite(var, index, value) -> let vX, vI = get_next_variable (), get_next_variable () in
            transfo (Apply ( Apply ( transformation_cont value ,
                Function_arg( vX , Apply ( Apply(transformation_cont index,
                    Function_arg (vI, Apply (Variable vEnv, ArrayWrite(var, Variable vI, Variable vX)))
                  ), Variable vEnvE ) )
              ), Variable vEnvE ) )
  | AMake(e) -> let vX = get_next_variable () in
            transfo ( Apply ( Apply ( transformation_cont e,
                Function_arg(vX, Apply(Variable vEnv, AMake(Variable vX) ))
               ) , Variable vEnvE ) )

  | Let_match(v1, v2, e1, e2) -> let vX = get_next_variable () in transfo (
      Apply(Apply(transformation_cont e1,
          Function_arg(vX, Let_match(v1, v2, Variable vX, Apply(Apply(transformation_cont e2, Variable vEnv), Variable vEnvE)))
        ), Variable vEnvE)
    )

  | _ -> raise Not_Implemented
