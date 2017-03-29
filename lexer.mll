{
  open Parser;;
  exception Eof;;
}

rule token = parse
    | [' ' '\t']            {   token lexbuf            }
    | ['0' '\n']            {   EOL                     }
    | eof                   {   raise Eof               }

    (* bool_expr *)
    | "<>"                  {   DIFFERENT               }
    | "||"                  {   OR                      }
    | "&&"                  {   AND                     }
    | "not"                 {   NOT                     }
    | "true"                {   TRUE                    }
    | "false"               {   FALSE                   }
    | "<>" {NEQ}

    (* arith_expr *)
    | '+'                   {PLUS}
    | '-'                   {MINUS}
    | '*'                   {TIMES}
    | '/'                   {DIVIDE}
    | ['0'-'9']+ as s   {   INT (int_of_string s)   }


    (* expr *)
    | '='                   {   EQUAL                   }
    | '('                   {   LPARENT                 }
    | ')'                   {   RPARENT                 }
    | "let"                 {   LET                     }
    | "in"                  {   IN                      }
    | ";;"                  {   END_PROG                }
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "fun" { FUN }
    | "->" {ARROW}

    (* var *)
    | ['a'-'z']['a'-'z' 'A'-'Z' '_' '1'-'9']* as s {   VAR( s )  }
