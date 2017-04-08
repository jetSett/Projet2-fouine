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
exception Not_A_Program of stack_value;;

let push_stack stack a =
  stack := a::!stack;;

let pop_stack stack = match !stack with
  | [] -> raise EmpyStack
  | x::q -> stack:=q; x;;

let pop_clot stack = let e = pop_stack stack in
  match e with
    | Clot(v, p, e) -> v, p, e
    | _ -> raise (Not_A_Closure e);;

let pop_int stack = let e = pop_stack stack in
  match e with
    | Int(i) -> i
    | _ -> raise (Not_An_Int e);;

let pop_bool stack = let e = pop_stack stack in
  match e with
    | Int(0) -> 0
    | Int(1) -> 1
    | _ -> raise (Not_A_Boolean e);;

let pop_env stack = let e = pop_stack stack in
  match e with
    | Env(e) -> e
    | _ -> raise (Not_An_Env e);;

let pop_prog stack = let e = pop_stack stack in
  match e with
    | Program(c) -> c
    | _ -> raise (Not_A_Program e);;

let bool_to_intruct b = if  b then Int(1) else Int(0);;

let stack = ref []

let env = ref (SECD_env.create ())

let rec interpret_SECD prog =
match prog with
  | [] -> ()
  | ADD::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (Int(a1 + a2)); interpret_SECD q
  | SUB::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (Int(a1 - a2)); interpret_SECD q
  | MUL::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (Int(a1 * a2)); interpret_SECD q
  | DIV::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (Int(a1 / a2)); interpret_SECD q
  | CONST(x)::q -> push_stack stack (Int(x)); interpret_SECD q
  (* BOOLEANS *)
  | AND::q -> let a1, a2 = pop_bool stack, pop_bool stack in push_stack stack (Int(a1*a2)); interpret_SECD q
  | OR::q -> let a1, a2 = pop_bool stack, pop_bool stack in push_stack stack (Int(a1+a2-a1*a2)); interpret_SECD q
  | NOT::q -> let a = pop_bool stack in push_stack stack (Int(1-a)); interpret_SECD q
  | EQ::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (bool_to_intruct (a1=a2)); interpret_SECD q
  | NEQ::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (bool_to_intruct (a1<>a2)); interpret_SECD q
  | LT::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (bool_to_intruct (a1<a2)); interpret_SECD q
  | GT::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (bool_to_intruct (a1>a2)); interpret_SECD q
  | LTE::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (bool_to_intruct (a1<=a2)); interpret_SECD q
  | GTE::q -> let a1, a2 = pop_int stack, pop_int stack in push_stack stack (bool_to_intruct (a1>=a2)); interpret_SECD q
  (* OTHER *)
  | LET(v)::q ->(* print_string "LET\n"; *)let a = pop_stack stack in SECD_env.push !env v a; interpret_SECD q
  | ACCESS(v)::q -> (*print_string "ACCESS\n"; *)let a = SECD_env.search !env v in push_stack stack a; interpret_SECD q
  | CLOS (v, prog)::q->(* print_string "CLOS\n"; *)push_stack stack (Clot(v, prog, SECD_env.copy !env)); interpret_SECD q
  | ENDLET(x)::q -> (*print_string "ENDLET\n"; *)SECD_env.pop !env x; interpret_SECD q
  | APPLY::q -> (*print_string "APPLY\n"; *)let x, prog, newEnv = pop_clot stack in let v = pop_stack stack in
                (* first, saving our state *)
                push_stack stack (Env(!env)); (* pushing the old env *)
                push_stack stack (Program(q)); (* pushing our state in the program *)
                (* then changing our state *)
                SECD_env.push newEnv x v; (* update the new env *)
                env := newEnv; (* changing our environment *)
                interpret_SECD prog (* continuing *)
  | RET::_ -> (*print_string "RET\n"; *)let v = (pop_stack stack) and c = (pop_prog stack) and e = (pop_env stack) in
                push_stack stack v; (* we add the return value on the stack *)
                env := e; (* we go back to the env *)
                interpret_SECD c (* we return to our execution *)
  | IF_THEN_ELSE::q -> let boolean = pop_bool stack and a = pop_stack stack and b = pop_stack stack in
                        push_stack stack (if boolean=1 then a else b);
                        interpret_SECD q
  | PR_INT::q -> let a = pop_int stack in print_int a; print_string "\n"; interpret_SECD q
