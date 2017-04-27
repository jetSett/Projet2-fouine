open Expression;;

exception Not_Implemented;;

(* upercase because this is forbiden to use upercase starting variables *)
let vEnv = Var("K") and vEnvE = Var("KE") and vX = Var("X") and vY = Var("Y")

let ident = Function_arg(vX, Variable(vX))
let transfo e = Function_arg(vEnv, e)
let transfo_0 e = transfo (Apply(Variable(vEnv), e))
let transfo_1 e finalX= transfo (Apply(e,
                                  Function_arg(vX,
                                    Apply(Variable(vEnv), finalX)
                                  )
                                ))

let transfo_2 e1 e2 finalXY = transfo (Apply(e1, Function_arg(vX,
                                    Apply(e2, Function_arg(vY,
                                        Apply(Variable(vEnv),  finalXY)
                                      ))
                                  )))

let rec transformation_cont expr = match expr with
  | Unit | Variable(_) | Const_int(_) | Const_bool(_) -> transfo_0 expr
  | Not(e) -> transfo_1 (transformation_cont e) (Not(Variable(vX)))
  | Lt(e1, e2) -> transfo_2 (transformation_cont e1) (transformation_cont e2) (Lt(Variable(vX), Variable(vY)))
  | Gt(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Gt(Variable(vX), Variable(vY)))
  | Lte(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Lte(Variable(vX), Variable(vY)))
  | Gte(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Gte(Variable(vX), Variable(vY)))
  | And(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (And(Variable(vX), Variable(vY)))
  | Or(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Or(Variable(vX), Variable(vY)))
  | Eq(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Eq(Variable(vX), Variable(vY)))
  | Neq(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Neq(Variable(vX), Variable(vY)))
  | Plus(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Plus(Variable(vX), Variable(vY)))
  | Minus(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Minus(Variable(vX), Variable(vY)))
  | Times(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Times(Variable(vX), Variable(vY)))
  | Divide(e1, e2) ->  transfo_2 (transformation_cont e1) (transformation_cont e2) (Divide(Variable(vX), Variable(vY)))
  (* | Let_in(var, e1, e2) -> transfo
                              (Apply(transformation_cont e1,
                                Function_arg(var,
                                  Apply(transformation_cont e2, Function_arg(vX,
                                    Apply(Variable vEnv, Variable vX)))
                                ))
                            ) *)
  | Let_in(var, e1, e2) -> transfo (Let_in(var, Apply(transformation_cont e1, ident), Apply(transformation_cont e2, Variable vEnv)))
  | Function_arg(var, e) ->  transfo (Function_arg(var, Apply(transformation_cont e, Variable vEnv)))

  | Apply(e1, e2) -> transfo (Apply(transformation_cont e2, Function_arg(vX,
                                Apply(Apply(transformation_cont e1, Variable vEnv), Variable vX)
                              )))

  | IfThenElse(b, e1, e2) -> transfo (Apply(transformation_cont b,
                                        Function_arg(vX, IfThenElse(Variable(vX),
                                            Apply(transformation_cont e1, Variable(vEnv)),
                                            Apply(transformation_cont e2, Variable(vEnv))
                                        ))
                                      ))
  | Let_rec(var, e1, e2) -> transfo (Let_rec(var, Apply(transformation_cont e1, ident), Apply(transformation_cont e2, Variable vEnv)))
  | PrInt(e) -> transfo (Apply(transformation_cont e, Function_arg(vX, Apply(Variable(vEnv), PrInt(Variable(vX))))))
  (*
  | TryWith(e1, var, e2) -> v
  | Raise(e) as -> v

  *)
  | _ -> raise Not_Implemented
