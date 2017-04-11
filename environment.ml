open Expression;;
open List;;

module type PushDictionary = sig
  type 'a dict (* un dictio indexé par des variables *)

  val create : unit -> 'a dict
  val push : 'a dict -> variable -> 'a -> unit
  val replace : 'a dict -> variable -> 'a -> unit
  val search : 'a dict -> variable -> 'a
  val mem : 'a dict -> variable -> bool
  val pop : 'a dict -> variable -> unit

  val copy : 'a dict -> 'a dict
  (* val print : 'a dict -> unit *)
end

module Environment =
functor (Dict : PushDictionary) ->
  struct
    type value =
      | Int of int
      | Closure of expr * value Dict.dict
      | RefInt of int ref
    ;;

    type env = value Dict.dict;;

    let create : unit -> env = Dict.create;;

    let push : env -> variable -> value -> unit = Dict.push;;

    let replace : env -> variable -> value -> unit = Dict.replace;;

    let search : env -> variable -> value = Dict.search;;

    let pop : env -> variable -> unit = Dict.pop;;

    let copy : env -> env = Dict.copy;;

    let printValue = function
      | Int(i) -> print_string "- : int = "; print_int i
      | Closure(f, e') -> print_string "- : function = "; PrintExpr.printExpr stdout f
      | RefInt(r) -> print_string "- : int ref = "; print_int (!r)
    ;;

    let env_free_var : env -> variable list -> env = fun env l ->
      let e = create () in
      let rec aux = function
        | [] -> ()
        | x::q -> push e x (search env x); aux q
      in aux l;
      e;;
    (*let env_free_var e l = copy e;;*)
  end
