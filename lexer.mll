{
  open Parser;;
}

rule token = parse
    | [' ' '\t' '\n']       {   token lexbuf            }
    | eof                   {   END_PROG                }

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
    | ";;"                  {   STP                     }
    | "if"                  {   IF                      }
    | "then"                {   THEN                    }
    | "else"                {   ELSE                    }
    | "fun"                 {   FUN                     }
    | "->"                  {   RARROW                  }
    | "<-"                  {   LARROW                  }
    | '.'                   {   POINT                   }
    | "ref"                 {   REF                     }
    | "!"                   {   DEREF                   }
    | ":="                  {   SET                     }
    | ";"                   {   IMP                     }
    | ","                   {   COMMA                   }
    | "try"                 {   TRY                     }
    | "with"                {   WITH                    }
    | "E"                   {   EXCEPT                  }
    | "raise"               {   RAISE                   }
    | "prInt"               {   PRINT                   }
    | "aMake"               {   AMAKE                   }

    (* var *)
    | ['a'-'z']['a'-'z' 'A'-'Z' '_' '1'-'9']* as s {   VAR( s )  }
