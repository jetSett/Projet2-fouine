open Expression;;
open Lexer;;
open PrintExpr;;
open Eval;;
open Environment;;

let lexbuf = Lexing.from_channel stdin in
let parse () = Parser.main Lexer.token lexbuf in

let result = parse () in
printExpr result;
print_newline ();

let value = eval (Env.create ()) result in
printValue value;
print_newline ();
