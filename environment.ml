open Expression;;
open List;;

type value =
      | Int of int
      | Closure of expr * env
and
  env = (variable*value) list;;

module Env = struct
  let create () = ref []
  let push e x v = e := (x, v)::(!e)
  let search e x = List.assoc x (!e)
  let pop e x = e := (List.remove_assoc x (!e))
end;;
