open Expression;;
open Lexer_type;;

let memoryMax = ref 4096;;
let size_max = string_of_int (!memoryMax);;

let mem_str =
  "let mem = aMake "^size_max^";;\n\n"
;;

(*Create a new index and return it*)
let allocate_str =
  "let allocate =
    (fun v ->
    let mem_size = mem.(0) <- (mem.(0) + 1) in
    let ignore_ = mem.(mem_size) <- v in mem_size);;\n\n"
;;

let read_str =
  "let read =
    (fun adr ->
    mem.(adr));;\n\n"
;;

let write_str =
  "let write =
    (fun adr -> fun v ->
    let ignore_ = mem.(adr) <- v in v);;\n\n"
;;

let lib = mem_str ^ allocate_str ^ read_str ^ write_str;;

let transform_imp e =
  let rec aux = function
    | Reference(r) -> "(let v = "^(aux r)^" in allocate v)" (*call allocate*)
    | Deference(r) -> "(let r = "^(aux r)^" in read r)" (*call read*)
    | Imp(a, b) -> "(let ignore_ = "^(aux a)^" in "^(aux b)^")" (* ignore the value of the first evaluation *)
    | Set(Var(adr), v) -> "(let adr, v = "^adr^", "^(aux v)^" in write adr v)" (*call write*)
    | Unit -> "()"
    | Raise(e) -> "(raise "^(aux e)^")"
    | PrInt(e) -> "(prInt "^(aux e)^")"
    | Variable(Var(x)) -> x
    | Let_in(Var(x), e1, e2) -> "(let "^x^" = "^(aux e1)^" in "^(aux e2)^")"
    | Let_rec(Var(x), e1, e2) -> "(let rec "^x^" = "^(aux e1)^" in "^(aux e2)^")"
    | Let_match(Var(x), Var(y), e1, e2) -> "(let "^x^", "^y^" = "^(aux e1)^" in "^(aux e2)^")"
    | Function_arg(Var(x), e) -> "(fun "^x^" -> "^(aux e)^")"
    | IfThenElse(c, a, b) -> "(if "^(aux c)^" then "^(aux a)^" else "^(aux b)^")"
    | TryWith(e1, Var(x), e2) -> "(try "^(aux e1)^"with E "^x^" -> "^(aux e2)^")"
    | Const_bool(b) -> if b then "true" else "false"
    | Not(c) -> "(not "^(aux c)^")"
    | And(a, b) -> "("^(aux a)^" && "^(aux b)^")"
    | Or(a, b) -> "("^(aux a)^" || "^(aux b)^")"
    | Eq(a, b) -> "("^(aux a)^" = "^(aux b)^")"
    | Neq(a, b) -> "("^(aux a)^" <> "^(aux b)^")"
    | Const_int(x) -> string_of_int x
    | Plus(a, b) -> "("^(aux a)^" + "^(aux b)^")"
    | Minus(a, b) -> "("^(aux a)^" - "^(aux b)^")"
    | Times(a, b) -> "("^(aux a)^"*"^(aux b)^")"
    | Divide(a, b) -> "("^(aux a)^"/"^(aux b)^")"
    | Apply(a, b) -> "("^(aux a)^" "^(aux b)^")"
    | Lt(a, b) -> "("^(aux a)^" < "^(aux b)^")"
    | Gt(a, b) -> "("^(aux a)^" > "^(aux b)^")"
    | Lte(a, b) -> "("^(aux a)^" <= "^(aux b)^")"
    | Gte(a, b) -> "("^(aux a)^" >= "^(aux b)^")"
    | AMake(e) -> "(aMake "^(aux e)^")"
    | ArrayAccess(Var(varTab), e2) -> "("^varTab^".("^(aux e2)^"))"
    | ArrayWrite(Var(varTab), e1, e2) -> "("^varTab^".("^(aux e1)^") <- "^(aux e2)^")"
    | Comma(a, b) -> "("^(aux a)^", "^(aux b)^")"
  in
  let prog = lib^(aux e)^";;" in
  let lexbuf = Lexing.from_string prog in
  let parse () = Parser.main Lexer.token lexbuf in t_conversion (parse ());;
