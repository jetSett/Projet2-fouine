open Program;;
open Lexer;;
open Print_prog;;

let lexbuf = Lexing.from_channel stdin in
let parse () = Parser.main Lexer.token lexbuf in
let result = parse () in
print_program result;;
print_string "\n";
