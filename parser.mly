%{
  open Expression;;
%}

%token EOL
%token END_PROG

// expr
%token LPARENT RPARENT
%token LET IN FUN ARROW IF THEN ELSE AFFECT
%token <string> VAR

// bool_expr
%token TRUE FALSE
%token EQ NEQ OR AND NOT

// arith_expr
%token <int> INT
%token PLUS MINUS DIVIDE TIMES

%left EQ NEQ
%left OR
%left AND
%nonassoc NOT

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */

%start main

%type <Expression.expr> main

%%

main:
  expr END_PROG { $1 }
;

variable:
  | VAR                                         {       Var($1)                           }
;

lvariable:
  |                                             {       []                                }
  | variable lvariable                          {       $1::$2                            }
;

sexpr:
  | LPARENT RPARENT                             {     Unit                                }
  | LPARENT expr RPARENT                        {     $2                                  }
  | LET variable lvariable AFFECT expr IN expr  {     Let_in($2, map_fun $3 $5, $7)       }
  | FUN variable lvariable ARROW expr           {     Function_arg($2, map_fun $3 $5)     }
  | variable                                    {     Variable($1)                        }
  | IF expr THEN expr ELSE expr                 {     IfThenElse($2, $4, $6)              }
  | expr PLUS expr                              {     Plus($1, $3)                        }
  | expr MINUS expr                             {     Minus($1, $3)                       }
  | expr TIMES expr                             {     Times($1, $3)                       }
  | expr DIVIDE expr                            {     Divide($1, $3)                      }
  | MINUS expr %prec UMINUS                     {     Minus(Const_int(0), $2)             }
  | INT                                         {     Const_int($1)                       }
  | TRUE                                        {     Const_bool(true)                    }
  | FALSE                                       {     Const_bool(false)                   }
  | NOT expr                                    {     Not($2)                             }
  | expr AND expr                               {     And($1, $3)                         }
  | expr OR expr                                {     Or($1, $3)                          }
  | expr EQ expr                                {     Eq($1, $3)                          }
  | expr NEQ expr                               {     Neq($1, $3)                         }
;

expr:
  | sexpr                                       {     $1                                  }
  | expr sexpr                                  {     Apply($1, $2)                       }
;
