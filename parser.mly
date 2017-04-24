%{
  open Expression;;
%}

%token EOF
%token STP END_PROG

// expr
%token LPARENT RPARENT
%token LET IN FUN RARROW LARROW POINT AMAKE IF THEN ELSE REF DEREF SET IMP REC AFFECT
%token TRY WITH EXCEPT RAISE PRINT
%token <string> VAR

// bool_expr
%token TRUE FALSE
%token EQ NEQ OR AND NOT GT LT GTE LTE

// arith_expr
%token <int> INT
%token PLUS MINUS DIVIDE TIMES

// priority

%left LET REC IN
%left TRY WITH

%left IMP
%nonassoc IF
%nonassoc THEN
%nonassoc ELSE
%right FUN
%nonassoc RARROW
%nonassoc LARROW
%left SET

%left OR
%left AND
%left LT GT LTE GTE
%left EQ NEQ
%nonassoc NOT

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%nonassoc REF
%nonassoc DEREF

%nonassoc PRINT
%nonassoc RAISE
%nonassoc EXCEPT

%left STP
%left ENDPROG
%left LPARENT RPARENT

%start main

%type <Expression.expr> main

%%

main:
  | dexpr STP          { $1 }
  | dexpr END_PROG     { $1 }
;

variable:
  | VAR                                         {       Var($1)                           }
;

lvariable:
  |                                             {       []                                }
  | variable lvariable                          {       $1::$2                            }
;

expr:
  | expr sexpr                                  {     Apply($1, $2)                       }
  | sexpr                                       {     $1                                  }
;

dexpr:
  | LET REC variable lvariable EQ expr STP dexpr {     Let_rec($3, map_fun $4 $6, $8)      }
  | LET variable lvariable EQ expr STP dexpr     {     Let_in($2, map_fun $3 $5, $7)       }
  | expr                                         {     $1                                  }

sexpr:
  | IF bexpr THEN expr ELSE expr                {     IfThenElse($2, $4, $6)              }
  | PRINT expr                                  {     PrInt($2)                           }
  | expr IMP expr                               {     Imp($1, $3)                         }

  | LET REC variable lvariable EQ expr IN expr  {     Let_rec($3, map_fun $4 $6, $8)      }
  | LET variable lvariable EQ expr IN expr      {     Let_in($2, map_fun $3 $5, $7)       }
  | FUN variable lvariable RARROW expr           {     Function_arg($2, map_fun $3 $5)     }

  | TRY expr WITH EXCEPT variable RARROW expr    {     TryWith($2, $5, $7)                 }
  | RAISE expr                                  {     Raise($2)                           }

  | REF expr                                    {     Reference($2)                       }
  | DEREF expr                                  {     Deference($2)                       }
  | variable SET expr                           {     Set($1, $3)                         }

  | AMAKE expr                                  {     AMake($2)                           }
  | variable POINT LPARENT expr RPARENT LARROW expr { TabWrite($1, $4, $7)                }
  | variable POINT LPARENT expr RPARENT             { TabAccess($1, $4)                   }

  | expr PLUS expr                              {     Plus($1, $3)                        }
  | expr MINUS expr                             {     Minus($1, $3)                       }
  | expr TIMES expr                             {     Times($1, $3)                       }
  | expr DIVIDE expr                            {     Divide($1, $3)                      }
  | MINUS expr %prec UMINUS                     {     Minus(Const_int(0), $2)             }

  | INT                                         {     Const_int($1)                       }
  | TRUE                                        {     Const_bool(true)                    }
  | FALSE                                       {     Const_bool(false)                   }
  | variable                                    {     Variable($1)                        }

  | LPARENT expr RPARENT                        {     $2                                  }
  | LPARENT RPARENT                             {     Unit                                }
;

bexpr:
  | expr EQ expr                              {     Eq($1, $3)                          }
  | expr NEQ expr                             {     Neq($1, $3)                         }
  | expr LT expr                              {     Lt($1, $3)                          }
  | expr GT expr                              {     Gt($1, $3)                          }
  | expr LTE expr                             {     Lte($1, $3)                         }
  | expr GTE expr                             {     Gte($1, $3)                         }
  | bexpr AND bexpr                           {     And($1, $3)                         }
  | bexpr OR bexpr                            {     Or($1, $3)                          }
  | NOT bexpr                                 {     Not($2)                             }
;
