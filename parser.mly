%{
  open Program;;
%}

%token EOL

// prog
%token LET IN FUN ARROW
%token LPARENT RPARENT END_PROG
%token <string> VAR


// bool_expr
%token TRUE FALSE
%token EQUAL DIFFERENT OR AND NOT


// arith_expr
%token <int> INT
%token PLUS MINUS DIVIDE TIMES

%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */

%left PLUS MINUS
%left TIMES DIVIDE

%start main

%type <Program.prog> main

%%

main:
  prog END_PROG { $1 }
;

variable:
  | VAR                                          { Var($1)                       }
;

prog:
  | LPARENT RPARENT                              {Nil}
  | LPARENT prog RPARENT                         { $2                             }
  | LET variable EQUAL bool_expr IN prog         { Let_x_boolexpr_in($2, $4, $6)  }
  | LET variable EQUAL arith_expr IN prog        { Let_x_arith_expr_in($2, $4, $6)}
  | LET variable EQUAL funct IN prog             { Let_x_function_in($2, $4, $6)}
  | eval                                         { Eval($1)                  }
;

funct:
  | FUN variable ARROW prog                      {Function_arg ($2, $4)}
  | FUN LPARENT RPARENT ARROW prog               {Function_noArg($5)}
;

arith_expr:
  | LPARENT arith_expr RPARENT                    {$2}
  | arith_expr PLUS arith_expr                    {Plus($1, $3)}
  | arith_expr MINUS arith_expr                   {Minus($1, $3)}
  | arith_expr TIMES arith_expr                   {Times($1, $3)}
  | arith_expr DIVIDE arith_expr                  {Divide($1, $3)}
  | MINUS arith_expr %prec UMINUS                 { Minus(Const_int(0), $2) }
  | INT {Const_int($1)}
  | eval {Eval_arith($1)}

;

bool_expr:
  | LPARENT bool_expr RPARENT                     {$2}
  | TRUE                                          {Const_bool(true)}
  | FALSE                                         {Const_bool(false)}
  | NOT bool_expr                                 {Not($2)}
  | bool_expr AND bool_expr                       {And($1, $3)}
  | bool_expr OR bool_expr                        {Or($1, $3)}
  | eval                                          {Eval_bool($1)}
;

eval:
  | LPARENT prog RPARENT                         {Eval_prog($2)}
  | variable {Eval_var($1)}
  | arith_expr                                   { Eval_arith_expr($1)}
  | bool_expr                                    { Eval_bool_expr($1)             }
  | funct_eval                                   {$1}
  | variable eval                                {Eval_function_var($1, $2)}

funct_eval:
  | LPARENT funct RPARENT eval                   {Eval_function_anon($2, $4)}
  | funct_eval eval                              {Eval_function_arg($1, $2)}
  | variable eval                                {Eval_function_var($1, $2)}
