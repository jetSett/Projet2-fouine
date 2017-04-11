open Lexer;;
open PrintExpr;;
open Eval;;
open Environment;;
open SECD_interpret;;
open SECD;;

let debug_enable = ref false;;
let machine_enable = ref false;;

let interm_enable = ref false;;
let interm_output = ref stdout;;
let print_output name =
  interm_enable := true;
  interm_output := (open_out name)
;;

let expr_input = ref stdin;;
let command = ref true;;
let set_input name =
  command := false;
  expr_input := (open_in name);;

let options_list = [
  ("-debug", (Arg.Set debug_enable), "Print the program");
  ("-machine", (Arg.Set machine_enable), "Run the SECD machine");
  ("-interm", (Arg.String print_output), "Produce the SECD program")
];;

let usage_msg = "Please read the rapport for further details";;

let run () =
  Arg.parse options_list set_input usage_msg;
  if !command then (
    print_string "\t\t\t\t Fouine version 1.00.0\n";
    print_string "\n";
    print_string "# ";
    flush stdout);

  let lexbuf = Lexing.from_channel (!expr_input) in
  let parse () = Parser.main Lexer.token lexbuf in

  let result = parse () in
  if not (!command) then close_in (!expr_input);

  if !debug_enable then
    begin
      printExpr stdout result;
      print_newline ()
    end;

  let value = eval (Env.create ()) result in
  Env.printValue value;
  print_newline ();

  if !machine_enable || !interm_enable then
    try
      let prog = compile result in
      if !debug_enable then (
        printSECD stdout prog
      );
      if !machine_enable then (
        interpret_SECD prog
      );
      if !interm_enable then (
        printSECD (!interm_output) prog;
        close_out (!interm_output)
      );
    with Not_Supported_Yet(e) as ex ->
      print_string "UNSUPPORTED : ";
      printExpr stdout e;
      raise ex
;;

run ();;
