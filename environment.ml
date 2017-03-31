type Value =
      | Int of int
      | Bool of bool
      | Cloture of expr * env
and
  env == string*int list;;
