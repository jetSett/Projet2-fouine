open Expression;;
open SECD;;
open SECD_environment;;
open Dictpush_hashTbl;;
open PrintExpr;;


module SECD_env = SECD_Environment(Dictpush_hashTbl);;

open SECD_env;;

exception EmpyStack;;
exception Not_A_Closure of stack_value;;
exception Not_An_Int of stack_value;;
exception Not_A_Boolean of stack_value;;
exception Not_An_Env of stack_value;;
exception Ret_Not_Possible;;
exception Not_A_Ref of stack_value;;
exception Raise_Not_Catched of int;;

let push_stack stack a =
  stack := a::!stack;;

let pop_stack stack = match !stack with
  | [] -> raise EmpyStack
  | x::q -> stack:=q; x;;

let pop_stack_clot stack = let e = pop_stack stack in
  match e with
    | Clot(v, p, e) -> v, p, e
    | _ -> raise (Not_A_Closure e);;

let pop_stack_int stack = let e = pop_stack stack in
  match e with
    | Int(i) -> i
    | _ -> raise (Not_An_Int e);;

let pop_stack_bool stack = let e = pop_stack stack in
  match e with
    | Int(0) -> 0
    | Int(1) -> 1
    | _ -> raise (Not_A_Boolean e);;

let pop_stack_env stack = let e = pop_stack stack in
  match e with
    | Env(e) -> e
    | _ -> raise (Not_An_Env e);;

(* the idea is to pop everything on the top of the stack until we are done (we encounter an environment) *)
let pop_stack_ret stack = let x = pop_stack stack in
  let rec aux s = match s with
    | Program(c)::Env(e)::q -> stack := q; c, e
    | _::q -> aux q
    | [] -> raise Ret_Not_Possible
    in
    let a, b = aux !stack in
  x, a, b

let pop_stack_ref stack = let e = pop_stack stack in
  match e with
    | RefInt(c) -> c
    | _ -> raise (Not_A_Ref e);;

let bool_to_intruct b = if b then Int(1) else Int(0);;

let stack = ref [] (* the stack of the SECD *)
let tryWithStack = ref [] (* env*variable*program *)
let env = ref (SECD_env.create ())

let rec interpret_SECD prog =
match prog with
  | [] -> ()
  | ADD::q -> let a1, a2 = pop_stack_int stack, pop_stack_int stack in push_stack stack (Int(a1 + a2)); interpret_SECD q
  | SUB::q -> let a2, a1 = pop_stack_int stack, pop_stack_int stack in push_stack stack (Int(a1 - a2)); interpret_SECD q
  | MUL::q -> let a1, a2 = pop_stack_int stack, pop_stack_int stack in push_stack stack (Int(a1 * a2)); interpret_SECD q
  | DIV::q -> let a2 = pop_stack_int stack in let a1 = pop_stack_int stack in push_stack stack (Int(a1 / a2)); interpret_SECD q
  | CONST(x)::q -> push_stack stack (Int(x)); interpret_SECD q
  (* BOOLEANS *)
  | AND::q -> let a1, a2 = pop_stack_bool stack, pop_stack_bool stack in push_stack stack (Int(a1*a2)); interpret_SECD q
  | OR::q -> let a1, a2 = pop_stack_bool stack, pop_stack_bool stack in push_stack stack (Int(a1+a2-a1*a2)); interpret_SECD q
  | NOT::q -> let a = pop_stack_bool stack in push_stack stack (Int(1-a)); interpret_SECD q
  | EQ::q -> let a1, a2 = pop_stack_int stack, pop_stack_int stack in push_stack stack (bool_to_intruct (a1=a2)); interpret_SECD q
  | NEQ::q -> let a1, a2 = pop_stack_int stack, pop_stack_int stack in push_stack stack (bool_to_intruct (a1<>a2)); interpret_SECD q
  | LT::q ->  let a1 = pop_stack_int stack in let a2 = pop_stack_int stack in push_stack stack (bool_to_intruct (a1<a2)); interpret_SECD q
  | GT::q ->  let a1 = pop_stack_int stack in let a2 = pop_stack_int stack in push_stack stack (bool_to_intruct (a1>a2)); interpret_SECD q
  | LTE::q -> let a1 = pop_stack_int stack in let a2 = pop_stack_int stack in push_stack stack (bool_to_intruct (a1<=a2)); interpret_SECD q
  | GTE::q -> let a1 = pop_stack_int stack in let a2 = pop_stack_int stack in push_stack stack (bool_to_intruct (a1>=a2)); interpret_SECD q
  (* OTHER *)
  | LET(v)::q ->let a = pop_stack stack in SECD_env.push !env v a; interpret_SECD q
  | LET_REC(f)::q -> let var, funct, envFonct = pop_stack_clot stack in
                     while (SECD_env.mem envFonct f) do
                      SECD_env.pop envFonct f; (* removing f from the environment of the function *)
                     done;
                     let nClot = Clot(var, funct, envFonct) in
                     SECD_env.push envFonct f nClot;
                     SECD_env.push !env f nClot;
                     interpret_SECD q
  | ACCESS(v)::q -> let a = SECD_env.search !env v in push_stack stack a; interpret_SECD q
  | CLOS (v, prog)::q-> let var = free_variable_list prog in
                        let varSansv = List.filter (fun a -> a<>v) var in
                        push_stack stack (Clot(v, prog, SECD_env.env_free_var !env varSansv));
                        interpret_SECD q
  | ENDLET(x)::q -> SECD_env.pop !env x; interpret_SECD q
  | APPLY::q -> let x, prog, newEnv = pop_stack_clot stack in let v = pop_stack stack in
                (* first, saving our state *)
                push_stack stack (Env(!env)); (* pushing the old env *)
                push_stack stack (Program(q)); (* pushing our state in the program *)
                (* then changing our state *)
                SECD_env.push newEnv x v; (* update the new env *)
                env := newEnv; (* changing our environment *)
                interpret_SECD prog (* continuing *)
  | RET::_ -> let v, c, e = (pop_stack_ret stack) in
                push_stack stack v; (* we add the return value on the stack *)
                env := e; (* we go back to the env *)
                interpret_SECD c (* we return to our execution *)
  | IF_THEN_ELSE(a, b)::q -> let boolean = pop_stack_bool stack in
                        let c = (if boolean<>0 then a else b) in
                        interpret_SECD (c@q)
  | PR_INT::q -> let a = pop_stack_int stack in print_int a; print_string "\n";
                        push_stack stack (Int a); interpret_SECD q
  | TRYWITH(x, p)::q -> push_stack tryWithStack (SECD_env.copy !env, x, p); interpret_SECD q
  | RAISE::q -> let value = pop_stack_int stack in
                  let nEnv, var, prog =
                  try
                    pop_stack tryWithStack
                  with EmpyStack -> raise (Raise_Not_Catched(value))
                  in
                  SECD_env.push nEnv var (Int(value)); (* value of the exception in the new environment*)
                  env := nEnv; (* restauring the environment *)
                  interpret_SECD prog (* going to the "with" part *)
  | REF::q -> let a = pop_stack_int stack in
              push_stack stack (RefInt(ref a)); interpret_SECD q
  | DEREF::q -> let r = pop_stack_ref stack in
              push_stack stack (Int(!r)); interpret_SECD q
  | SET(v)::q -> let a = match SECD_env.search !env v with
                            | RefInt(r) -> r
                            | _ as r -> raise (Not_A_Ref(r))
                      in
                let value = pop_stack_int stack in
                a := value; interpret_SECD q
