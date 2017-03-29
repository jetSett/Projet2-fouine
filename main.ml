open Expression;;
open Lexer;;

let lexbuf = Lexing.from_channel stdin in
let parse () = Parser.main Lexer.token lexbuf in
let result = parse () in
print_string "\n";
