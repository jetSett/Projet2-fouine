%{
  open Program;;
%}

%token EOL

// expr
%token LPARENT RPARENT END_PROG
%token LET IN FUN ARROW IF THEN ELSE
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

%left OR
%left AND
%nonassoc NOT

%start main

%type <Program.expr> main

%%

main:
  prog END_PROG { $1 }
;

variable:
  | VAR                                          { Var($1)                       }
;

expr:
  | LPARENT RPARENT                              {Nil}
  | LPARENT expr RPARENT                         { $2                             }
  | LET variable EQUAL expr IN expr              { Let_in($2, $4, $6)  }
  | bool_expr                                    { BoolExpr($1)                  }
  | arith_expr                                   {ArithExpr($1)}
  | FUN variable ARROW expr {Function_arg($2, $4)}
  | variable {Variable($1)}
  | IF bool_expr THEN expr ELSE expr { IfThenElse($2, $4, $5) }

;

arith_expr:
  | LPARENT arith_expr RPARENT                    {$2}
  | expr PLUS expr                    {Plus($1, $3)}
  | expr MINUS expr                   {Minus($1, $3)}
  | expr TIMES expr                   {Times($1, $3)}
  | expr DIVIDE expr                  {Divide($1, $3)}
  | MINUS expr %prec UMINUS           { Minus(ArithExpr(Const_int(0)), $2) }
  | INT {Const_int($1)}

;

bool_expr:
  | LPARENT bool_expr RPARENT                     {$2}
  | TRUE                                          {Const_bool(true)}
  | FALSE                                         {Const_bool(false)}
  | NOT expr                                      {Not($2)}
  | expr AND expr                                 {And($1, $3)}
  | expr OR expr                                  {Or($1, $3)}
  | expr EQ expr                                  {Eq($1, $3)}
  | expr NEQ expr                                 {NEq($1, $3)}
;
