open Expression

exception Type_Mismatch

type fouine_type = Nothing_t | Int_t | Ref_t of fouine_type | Tab_t of fouine_type
		   | Pair_t of fouine_type * fouine_type
		   | Funct_t of fouine_type * fouine_type (* arg -> return *)
;;

type typed_expr =
  | T_Unit
  | T_Var of variable*fouine_type
  | T_Const_int of int
  | T_Const_bool of bool
  | T_Variable of variable
  | T_Let_in of variable * fouine_type * typed_expr * typed_expr
  | T_Let_rec of variable * fouine_type * typed_expr * typed_expr
  | T_Let_match of variable * fouine_type * variable * fouine_type * typed_expr * typed_expr
  | T_Function_arg of variable * fouine_type  * typed_expr * fouine_type
  | T_Not of typed_expr
  | T_Raise of typed_expr
  | T_IfThenElse of typed_expr * typed_expr * typed_expr
  | T_TryWith of typed_expr * variable * typed_expr (* variable non typée *)
  | T_AMake of typed_expr
  | T_ArrayAccess of variable * typed_expr
  | T_ArrayWrite of variable * typed_expr * typed_expr (* var.( e1 ) <- e2 *)
  | T_PrInt of typed_expr
  | T_And of typed_expr * typed_expr
  | T_Or of typed_expr * typed_expr
  | T_Eq of typed_expr * typed_expr
  | T_Neq of typed_expr * typed_expr
  | T_Plus of typed_expr * typed_expr
  | T_Minus of typed_expr * typed_expr
  | T_Times of typed_expr * typed_expr
  | T_Divide of typed_expr * typed_expr
  | T_Apply of typed_expr * typed_expr
  | T_Reference of typed_expr
  | T_Deference of typed_expr
  | T_Imp of typed_expr * typed_expr
  | T_Set of variable * typed_expr
  | T_Lt of typed_expr * typed_expr
  | T_Gt of typed_expr * typed_expr
  | T_Lte of typed_expr * typed_expr
  | T_Gte of typed_expr * typed_expr
  | T_Comma of typed_expr * typed_expr
;;

let map_fun variables expr =
  let rec aux = function
    | [] -> expr
    | T_Var(v, t)::xs -> T_Function_arg(v, t, aux xs, Nothing_t)
    | _ -> failwith "Erreur d'implémentation map_fun"
  in aux variables;;


let rec type_correct t e = match t, e with
  | Nothing_t, _ -> ()
  | _, Nothing_t -> ()
  | a, b when a=b-> ()
  | Ref_t(t1), Ref_t(t2) -> type_correct t1 t2
  | Tab_t(t1), Tab_t(t2) -> type_correct t1 t2
  | Funct_t(t1, t2), Funct_t(u1, u2) -> (type_correct t1 u1); (type_correct t2 u2)
  | Pair_t(t1, t2), Pair_t(u1, u2) -> (type_correct t1 u1); (type_correct t2 u2)
  | _ -> raise Type_Mismatch

let variable_types = ref []

let rec check_apply funct = (* we only manage the call by variable *)
  let rec liste_arg f l = match f with (* returns the variable of the function and the list of its args*)
    | T_Apply(T_Variable(v), e) -> v, (e::l)
    | T_Apply(e, x) -> liste_arg e (x::l)
    | _ -> Var(""), []
  in
  (* to check the type of the remainings arguments *)
  let rec check_all_nothing l = List.iter (fun x -> check_types x Nothing_t) l in 
  (* check if the type of the args are correct*)
  let rec check_types_args lArgs t_funct = match lArgs, t_funct with 
    | [], _ -> t_funct
    | x::q, Funct_t(t, e) -> 
      check_types x t;
      check_types_args q e
    | x::q, Nothing_t -> check_all_nothing lArgs; Nothing_t
    | _ -> failwith "Too many arguments"
  in
  let vf, lArgs = liste_arg funct [] in
  if lArgs = [] then Nothing_t (* anonymous function *)
  else
    let t_funct = List.assoc vf !variable_types in
    check_types_args lArgs t_funct
      
and
(* raise exception if type mismatch *)
 check_types e expect = match e with
  | T_Unit -> ()
  | T_Const_int _ -> type_correct Int_t expect
  | T_Const_bool _ -> type_correct Int_t expect
  | T_Variable(v) -> type_correct (List.assoc v !variable_types) expect
  | T_Let_in(var, var_type, e1, e2) ->
    check_types e1 var_type;
    variable_types := (var, var_type)::!variable_types;
    check_types e2 expect;
    variable_types := List.remove_assoc var !variable_types;
  | T_Let_rec(var, var_type, e1, e2) ->
    variable_types := (var, var_type)::!variable_types; (* declared before the type evaluation *)
    check_types e1 var_type;
    check_types e2 expect;
    variable_types := List.remove_assoc var !variable_types;

  | T_Let_match(v1, t1, v2, t2, e1, e2) ->
    check_types e1 (Pair_t(t1, t2));
    variable_types := (v1, t1)::(v2, t2)::!variable_types;
    check_types e2 expect;
    variable_types := List.remove_assoc v1 !variable_types;
    variable_types := List.remove_assoc v2 !variable_types
  | T_Function_arg(var, type_var, e, type_ret) ->
    variable_types := (var, type_var)::!variable_types;
    check_types e type_ret;
    variable_types := List.remove_assoc var !variable_types;
    type_correct (Funct_t(type_var, type_ret)) expect
  | T_Not(e) ->
    check_types e Int_t;
    type_correct Int_t expect;
  | T_Raise(e) ->
    check_types e Int_t;
  | T_IfThenElse(b, e1, e2) ->
    check_types b Int_t;
    check_types e1 expect;
    check_types e2 expect
  | T_TryWith(e1, var, e2) ->
    check_types e1 expect;
    variable_types := (var, Int_t)::!variable_types;
    check_types e2 expect;
    variable_types := List.remove_assoc var !variable_types;
  | T_AMake(e) ->
    check_types e Int_t;
    type_correct (Tab_t(Nothing_t)) expect
  | T_ArrayAccess(v, e) -> (
    check_types e Int_t;
    match List.assoc v !variable_types with
    | Nothing_t -> ()
    | Tab_t(t) -> type_correct t expect
    | _ -> raise Type_Mismatch
  )
  | T_ArrayWrite(v, e1, e2) -> (
    check_types e1 Int_t;
    match List.assoc v !variable_types with
    | Nothing_t -> check_types e2 expect
    | Tab_t(t) ->
      type_correct t expect;
      check_types e2 t
    | _ -> raise Type_Mismatch

  )
  | T_PrInt(e) -> check_types e Int_t
  | T_And(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Or(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Plus(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Minus(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Times(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Divide(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Lt(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Gt(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Lte(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Gte(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Neq(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Eq(e1, e2) ->
    check_types e1 Int_t;
    check_types e2 Int_t;
    type_correct Int_t expect
  | T_Apply(_) as e -> type_correct (check_apply e) expect
  | T_Var(_) -> failwith "Using tvar out of lexing"
  | T_Deference(e) -> check_types e (Ref_t(expect))
  | T_Reference(e) -> (
    match expect with
      | Ref_t(t) -> check_types e t
      | Nothing_t -> check_types e Nothing_t
      | _ -> raise Type_Mismatch
    )
  | T_Imp(e1, e2) -> 
    check_types e1 Nothing_t;
    check_types e2 expect
  | T_Set(v, e) -> (
    match List.assoc v !variable_types with
      | Nothing_t -> check_types e Nothing_t
      | Ref_t(t) -> check_types e t;
                    type_correct t expect
      | _ -> raise Type_Mismatch
  )
  | T_Comma(e1, e2) -> (
    match expect with
      | Nothing_t -> check_types e1 Nothing_t;
                     check_types e2 Nothing_t
      | Pair_t(t1, t2) -> 
              check_types e1 t1;
              check_types e2 t2
      | _ -> raise Type_Mismatch
  )


    
let rec t_conversion = function
  | T_Unit -> Unit
  | T_Var _ -> failwith "Erreur d'implémentation t_conversion"
  | T_Const_int(a) -> Const_int(a)
  | T_Const_bool(a) -> Const_bool(a)
  | T_Variable(v) -> Variable(v)
  | T_Let_in(v, _, e1, e2) -> Let_in(v, t_conversion e1, t_conversion e2)
  | T_Let_rec(v, _, e1, e2) -> Let_rec(v, t_conversion e1, t_conversion e2)
  | T_Let_match(v1, _, v2, _, e1, e2) -> Let_match(v1, v2, t_conversion e1, t_conversion e2)
  | T_Function_arg(v, _, e, _) -> Function_arg(v, t_conversion e)
  | T_Not(e) -> Not(t_conversion e)
  | T_Raise(e) -> Raise(t_conversion e)
  | T_IfThenElse(e1, e2, e3) -> IfThenElse(t_conversion e1, t_conversion e2, t_conversion e3)
  | T_TryWith(e1, v, e2) -> TryWith(t_conversion e1, v, t_conversion e2)
  | T_AMake(e) -> AMake(t_conversion e)
  | T_ArrayAccess(v, e) -> ArrayAccess(v, t_conversion e)
  | T_ArrayWrite(v, e1, e2) -> ArrayWrite(v, t_conversion e1, t_conversion e2)
  | T_PrInt(e) -> PrInt(t_conversion e)
  | T_And(e1, e2) -> And(t_conversion e1, t_conversion e2)
  | T_Or(e1, e2) -> Or(t_conversion e1, t_conversion e2)
  | T_Eq(e1, e2) -> Eq(t_conversion e1, t_conversion e2)
  | T_Neq(e1, e2) -> Neq(t_conversion e1, t_conversion e2)
  | T_Plus(e1, e2) -> Plus(t_conversion e1, t_conversion e2)
  | T_Minus(e1, e2) -> Minus(t_conversion e1, t_conversion e2)
  | T_Times(e1, e2) -> Times(t_conversion e1, t_conversion e2)
  | T_Divide(e1, e2) -> Divide(t_conversion e1, t_conversion e2)
  | T_Apply(e1, e2) -> Apply(t_conversion e1, t_conversion e2)
  | T_Imp(e1, e2) -> Imp(t_conversion e1, t_conversion e2)
  | T_Lt(e1, e2) -> Lt(t_conversion e1, t_conversion e2)
  | T_Gt(e1, e2) -> Gt(t_conversion e1, t_conversion e2)
  | T_Lte(e1, e2) -> Lte(t_conversion e1, t_conversion e2)
  | T_Gte(e1, e2) -> Gte(t_conversion e1, t_conversion e2)
  | T_Comma(e1, e2) -> Comma(t_conversion e1, t_conversion e2)
  | T_Reference(e) -> Reference(t_conversion e)
  | T_Deference(e) -> Deference(t_conversion e)
  | T_Set(v, e) -> Set(v, t_conversion e);;
