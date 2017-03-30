{
  open Parser;;
}

rule token = parse
    | [' ' '\t' '\n']       {   token lexbuf            }

    (* bool_expr *)
    | "||"                  {   OR                      }
    | "&&"                  {   AND                     }
    | "not"                 {   NOT                     }
    | "true"                {   TRUE                    }
    | "false"               {   FALSE                   }
    | "=="                  {   EQ                      }
    | "<>"                  {   NEQ                     }

    (* arith_expr *)
    | '+'                   {   PLUS                    }
    | '-'                   {   MINUS                   }
    | '*'                   {   TIMES                   }
    | '/'                   {   DIVIDE                  }
    | ['0'-'9']+ as s       {   INT (int_of_string s)   }

    (* expr *)
    | '='                   {   AFFECT                  }
    | '('                   {   LPARENT                 }
    | ')'                   {   RPARENT                 }
    | "let"                 {   LET                     }
    | "in"                  {   IN                      }
    | ";;"                  {   END_PROG                }
    | "if"                  {   IF                      }
    | "then"                {   THEN                    }
    | "else"                {   ELSE                    }
    | "fun"                 {   FUN                     }
    | "->"                  {   ARROW                   }

    (* var *)
    | ['a'-'z']['a'-'z' 'A'-'Z' '_' '1'-'9']* as s {   VAR( s )  }
