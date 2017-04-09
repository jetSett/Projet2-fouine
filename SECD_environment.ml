open Expression;;
open Environment;;
open SECD;;

module SECD_Environment =
functor (Dict : PushDictionary) ->
  struct
    type stack_value =
        Int of int
      | Env of stack_value Dict.dict
      | Clot of variable * secd_program * stack_value Dict.dict
      | Program of secd_program
      | RefInt of int ref


    type value = stack_value;;

    type env = value Dict.dict;;

    let create : unit -> env = Dict.create;;

    let push : env -> variable -> value -> unit = Dict.push;;

    let replace : env -> variable -> value -> unit = Dict.replace;;

    let search : env -> variable -> value = Dict.search;;

    let pop : env -> variable -> unit = Dict.pop;;

    let copy : env -> env = Dict.copy;;

    let env_free_var : env -> variable list -> env = fun env l ->
      let e = create () in
      let rec aux = function
        | [] -> ()
        | x::q -> push e x (search env x); aux q
      in aux l;
      e;;
    (*let env_free_var e l = copy e;;*)
  end
