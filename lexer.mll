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
    | "="                   {   EQ                      }
    | "<>"                  {   NEQ                     }
    | ">"                   {   GT                      }
    | "<"                   {   LT                      }
    | ">="                  {   GTE                     }
    | "<="                  {   LTE                     }

    (* arith_expr *)
    | '+'                   {   PLUS                    }
    | '-'                   {   MINUS                   }
    | '*'                   {   TIMES                   }
    | '/'                   {   DIVIDE                  }
    | ['0'-'9']+ as s       {   INT (int_of_string s)   }

    (* expr *)
    | '('                   {   LPARENT                 }
    | ')'                   {   RPARENT                 }
    | "let"                 {   LET                     }
    | "rec"                 {   REC                     }
    | "in"                  {   IN                      }
    | ";;"                  {   END_PROG                }
    | "if"                  {   IF                      }
    | "then"                {   THEN                    }
    | "else"                {   ELSE                    }
    | "fun"                 {   FUN                     }
    | "->"                  {   ARROW                   }
    | "ref"                 {   REF                     }
    | "!"                   {   DEREF                   }
    | ":="                  {   SET                     }
    | ";"                   {   IMP                     }
    | "try"                 {   TRY                     }
    | "with"                {   WITH                    }
    | "E"                   {   EXCEPT                   }
    | "raise"               {   RAISE                   }

    (* var *)
    | ['a'-'z']['a'-'z' 'A'-'Z' '_' '1'-'9']* as s {   VAR( s )  }
