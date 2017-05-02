open Expression;;

exception Not_Implemented;;

(* upercase because this is forbiden to use upercase starting variables *)
let vEnv = Var("K") and vEnvE = Var("KE")

let indexVar = ref 0
let get_next_variable () = incr indexVar; Var(String.concat "" ["V"; (string_of_int !indexVar)])

let ident = Function_arg(Var("X"), Variable(Var("X")))

(* TODO : écrire replace_venv pour avoir des variables différentes à chaques fois, et continuer avec l'implémentation sympas *)

let rec replace_vEnv var e = match e with
| Unit | Const_int(_) | Const_bool(_) -> e
| Variable(v) when v = vEnv -> Variable(var)
| Variable(v) -> e
| Let_in(v, e1, e2) -> Let_in(v, replace_vEnv var e1, replace_vEnv var  e2)
| Let_rec(v, e1, e2) -> Let_rec(v, replace_vEnv var  e1, replace_vEnv var  e2)
| Let_match(v1, v2, e1, e2) -> Let_match(v1, v2, replace_vEnv var  e1, replace_vEnv var  e2)
| Function_arg(v, e) -> Function_arg(v, replace_vEnv var  e)
| Not(e) -> Not(replace_vEnv var  e)
| IfThenElse(a, b, c) -> IfThenElse(replace_vEnv var  a, replace_vEnv var  b, replace_vEnv var  c)
| PrInt(e) -> PrInt(replace_vEnv var  e)
| And(e1, e2) -> And(replace_vEnv var  e1, replace_vEnv var  e2)
| Or(e1, e2) -> Or(replace_vEnv var  e1, replace_vEnv var  e2)
| Eq(e1, e2) -> Eq(replace_vEnv var  e1, replace_vEnv var  e2)
| Neq(e1, e2) -> Neq(replace_vEnv var  e1, replace_vEnv var  e2)
| Plus(e1, e2) -> Plus(replace_vEnv var  e1, replace_vEnv var  e2)
| Minus(e1, e2) -> Minus(replace_vEnv var  e1, replace_vEnv var  e2)
| Times(e1, e2) -> Times(replace_vEnv var  e1, replace_vEnv var  e2)
| Divide(e1, e2) -> Divide(replace_vEnv var  e1, replace_vEnv var  e2)
| Apply(e1, e2) -> Apply(replace_vEnv var  e1, replace_vEnv var  e2)
| Lt(e1, e2) -> Lt(replace_vEnv var  e1, replace_vEnv var  e2)
| Gt(e1, e2) -> Gt(replace_vEnv var  e1, replace_vEnv var  e2)
| Lte(e1, e2) -> Lte(replace_vEnv var  e1, replace_vEnv var  e2)
| Gte(e1, e2) -> Gte(replace_vEnv var  e1, replace_vEnv var  e2)
| Comma(e1, e2) -> Comma(replace_vEnv var  e1, replace_vEnv var  e2)
| _ -> raise Not_Implemented

let transfo e = let vEnv = get_next_variable () in
                Function_arg(vEnv, replace_vEnv vEnv e)

let transfo_0 e = transfo (Apply(Variable(vEnv), e))
let transfo_1 e vX finalX= transfo (Apply(e,
                                  Function_arg(vX,
                                    Apply(Variable(vEnv), finalX)
                                  )
                                ))

let transfo_2 e1 e2 vX vY finalXY = transfo (Apply(e1, Function_arg(vX,
                                    Apply(e2, Function_arg(vY,
                                        Apply(Variable(vEnv),  finalXY)
                                      ))
                                  )))

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
  (* | Let_in(var, e1, e2) -> let vX = get_next_variable () in transfo
                              (Apply(transformation_cont e1,
                                Function_arg(var,
                                  Apply(transformation_cont e2, Function_arg(vX,
                                    Apply(Variable vEnv, Variable vX)))
                                ))
                            ) *)
  | Let_in(var, e1, e2) -> transfo (Let_in(var, transformation_cont e1, Apply(transformation_cont e2, Variable vEnv)))
  | Let_rec(var, e1, e2) -> transfo (Let_rec(var, transformation_cont e1, Apply(transformation_cont e2, Variable vEnv)))
  | Function_arg(var, e) ->  transfo (Function_arg(var, (Apply(transformation_cont e, Variable vEnv))))

  | Apply(f, e) -> let vX, vF = get_next_variable (), get_next_variable () in
                              transfo (Apply(transformation_cont f, Function_arg(vF,
                                Apply(transformation_cont e, Function_arg(vX,
                                  Apply(Apply(Variable(vF), Variable(vEnv)), Variable(vX))
                                ))
                              )))

  | IfThenElse(b, e1, e2) -> let vX = get_next_variable () in transfo (Apply(transformation_cont b,
                                        Function_arg(vX, IfThenElse(Variable(vX),
                                            Apply(transformation_cont e1, Variable(vEnv)),
                                            Apply(transformation_cont e2, Variable(vEnv))
                                        ))
                                      ))
  | PrInt(e) -> let vX = get_next_variable () in transfo (Apply(transformation_cont e, Function_arg(vX, Apply(Variable(vEnv), PrInt(Variable(vX))))))
  (*
  | TryWith(e1, var, e2) -> v
  | Raise(e) as -> v

  *)
  | _ -> raise Not_Implemented
