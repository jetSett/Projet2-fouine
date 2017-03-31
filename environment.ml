open Expression;;

type value =
      | Int of int
      | Closure of expr * env
and
  env = (string*int) list;;
