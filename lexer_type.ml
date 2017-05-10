open Expression

type fouine_type = Nothing_t | Int_t | Ref_t of fouine_type | Tab_t of fouine_type
  | Funct_t of fouine_type * fouine_type (* arg -> return *)
;;

type typed_variable = T_Var of string*fouine_type;;

type typed_expr =
  | T_Unit
  | T_Const_int of int
  | T_Const_bool of bool
  | T_Variable of typed_variable
  | T_Let_in of typed_variable * typed_expr * typed_expr
  | T_Let_rec of typed_variable * typed_expr * typed_expr
  | T_Let_match of typed_variable * typed_variable * typed_expr * typed_expr
  | T_Function_arg of typed_variable * typed_expr * fouine_type
  | T_Not of typed_expr
  | T_Raise of typed_expr
  | T_IfThenElse of typed_expr * typed_expr * typed_expr
  | T_TryWith of typed_expr * typed_variable * typed_expr
  | T_AMake of typed_expr
  | T_ArrayAccess of typed_variable * typed_expr
  | T_ArrayWrite of typed_variable * typed_expr * typed_expr (* var.( e1 ) <- e2 *)
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
  | T_Set of typed_variable * typed_expr
  | T_Lt of typed_expr * typed_expr
  | T_Gt of typed_expr * typed_expr
  | T_Lte of typed_expr * typed_expr
  | T_Gte of typed_expr * typed_expr
  | T_Comma of typed_expr * typed_expr
;;

let map_fun variables expr =
  let rec aux = function
    | [] -> expr
    | v::xs -> T_Function_arg(v, aux xs, Nothing_t)
  in aux variables;;


let t_var_conv v = match v with T_Var(e, _) -> Var(e)

let check_types e = ();; (* raise exception if type mismatch *)

let rec t_conversion = function
| T_Unit -> Unit
| T_Const_int(a) -> Const_int(a)
| T_Const_bool(a) -> Const_bool(a)
| T_Variable(v) -> Variable(t_var_conv v)
| T_Let_in(v, e1, e2) -> Let_in(t_var_conv v, t_conversion e1, t_conversion e2)
| T_Let_rec(v, e1, e2) -> Let_rec(t_var_conv v, t_conversion e1, t_conversion e2)
| T_Let_match(v1, v2, e1, e2) -> Let_match(t_var_conv v1, t_var_conv v2, t_conversion e1, t_conversion e2)
| T_Function_arg(v, e, _) -> Function_arg(t_var_conv v, t_conversion e)
| T_Not(e) -> Not(t_conversion e)
| T_Raise(e) -> Raise(t_conversion e)
| T_IfThenElse(e1, e2, e3) -> IfThenElse(t_conversion e1, t_conversion e2, t_conversion e3)
| T_TryWith(e1, v, e2) -> TryWith(t_conversion e1, t_var_conv v, t_conversion e2)
| T_AMake(e) -> AMake(t_conversion e)
| T_ArrayAccess(v, e) -> ArrayAccess(t_var_conv v, t_conversion e)
| T_ArrayWrite(v, e1, e2) -> ArrayWrite(t_var_conv v, t_conversion e1, t_conversion e2)
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
| T_Set(v, e) -> Set(t_var_conv v, t_conversion e);;
