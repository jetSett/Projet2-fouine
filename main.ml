open Lexer;;
open PrintExpr;;
open Eval;;
open Environment;;
open SECD_interpret;;
open SECD;;

let lexbuf = Lexing.from_channel stdin in
let parse () = Parser.main Lexer.token lexbuf in

let result = parse () in
printExpr result;
print_newline ();

let value = eval (Env.create ()) result in
Env.printValue value;
print_newline ();

try
  let prog = compile result in
  printSECD prog;
  print_newline ();
  interpret_SECD prog;
with Not_Supported_Yet(e) as ex ->
  print_string "UNSUPPORTED : ";
  printExpr e;
  print_newline ();
  raise ex
