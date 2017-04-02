open Expression;;

module HashedVariable = struct
  type t = variable
  let equal = (=)
  let hash = Hashtbl.hash
end

module Tbl = Hashtbl.Make(HashedVariable)

module Dictpush_hashTbl = struct
  type 'a dict = 'a Tbl.t
  let create () = Tbl.create 0
  let push e x v = Tbl.add e x v
  let search e x = Tbl.find e x
  let pop e x = Tbl.remove e x
  let copy e = Tbl.copy e
end;;