open Expression;;

module Dictpush_list = struct
  type 'a dict = ((variable*'a) list) ref
  let create () = ref []
  let push e x v = e := (x, v)::(!e)
  let search e x = List.assoc x (!e)
  let pop e x = e := (List.remove_assoc x (!e))
  let copy l =
    let rec aux = function
      | [] -> []
      | x::q -> x::(aux q)
      in
      let l2 = aux !l in
      ref l2
end;;
