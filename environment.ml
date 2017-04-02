open Expression;;
open List;;

module type PushDictionary = sig
  type 'a dict (* un dictio indexé par des variables *)

  val create : unit -> 'a dict
  val push : 'a dict -> variable -> 'a -> unit
  val search : 'a dict -> variable -> 'a
  val pop : 'a dict -> variable -> unit

  val copy : 'a dict -> 'a dict
  val print : 'a dict -> unit
end

module Environment =
functor (Dict : PushDictionary) ->
struct
  type value =
  | Int of int
  | Closure of expr * value Dict.dict

  type env = value Dict.dict

  let create : unit -> env = Dict.create

  let push : env -> variable -> value -> unit = Dict.push

  let search : env -> variable -> value = Dict.search

  let pop : env -> variable -> unit = Dict.pop

  let copy : env -> env = Dict.copy

  let printValue = function
    | Int(i) -> print_string "- : int = "; print_int i
    | Closure(f, e') -> print_string "- : function = "; PrintExpr.printExpr f;

  let env_free_var env l = let e = create () in
  let rec aux = function
    | [] -> ()
    | x::q -> push e x (search env x); aux q;
    in
  aux l; e

end;;
