open Expression;;
open Lexer;;
open PrintExpr;;

let lexbuf = Lexing.from_channel stdin in
let parse () = Parser.main Lexer.token lexbuf in
let result = parse () in
printExpr result;
print_string "\n";
