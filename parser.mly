%{
  open Expression;;
%}

%token EOF
%token STP END_PROG

// expr
%token LPARENT RPARENT
%token LET IN FUN ARROW IF THEN ELSE REF DEREF SET IMP REC AFFECT
%token TRY WITH EXCEPT RAISE PRINT
%token <string> VAR

// bool_expr
%token TRUE FALSE
%token EQ NEQ OR AND NOT GT LT GTE LTE

// arith_expr
%token <int> INT
%token PLUS MINUS DIVIDE TIMES

%left LET, REC, IN
%left IMP
%left IF, THEN, ELSE
%right FUN, ARROW
%left SET

%left OR
%left AND
%left EQ NEQ LT GT LTE GTE
%nonassoc NOT

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%nonassoc DEREF

%start main

%type <Expression.expr> main

%%

main:
  | expr STP      { $1 }
  | expr END_PROG { $1 }
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
  | RAISE INT                                   {     Raise($2)                           }
  | PRINT expr                                  {     PrInt($2)                           }
  | TRY expr WITH EXCEPT variable ARROW expr    {     TryWith($2, $5, $7)                 }
  | LET REC variable lvariable EQ expr STP expr {     Let_rec($3, map_fun $4 $6, $8)      }
  | LET variable lvariable EQ expr STP expr     {     Let_in($2, map_fun $3 $5, $7)       }
  | LET REC variable lvariable EQ expr IN expr  {     Let_rec($3, map_fun $4 $6, $8)      }
  | LET variable lvariable EQ expr IN expr      {     Let_in($2, map_fun $3 $5, $7)       }
  | FUN variable lvariable ARROW expr           {     Function_arg($2, map_fun $3 $5)     }
  | variable SET expr                           {     Set($1, $3)                         }
  | variable                                    {     Variable($1)                        }
  | expr IMP expr                               {     Imp($1, $3)                         }
  | REF expr                                    {     Reference($2)                       }
  | IF bexpr THEN expr ELSE expr                {     IfThenElse($2, $4, $6)              }
  | DEREF expr                                  {     Deference($2)                       }
  | expr PLUS expr                              {     Plus($1, $3)                        }
  | expr MINUS expr                             {     Minus($1, $3)                       }
  | expr TIMES expr                             {     Times($1, $3)                       }
  | expr DIVIDE expr                            {     Divide($1, $3)                      }
  | MINUS expr %prec UMINUS                     {     Minus(Const_int(0), $2)             }
  | INT                                         {     Const_int($1)                       }
  | TRUE                                        {     Const_bool(true)                    }
  | FALSE                                       {     Const_bool(false)                   }
;

bexpr:
  | expr                                        {     $1                                  }
  | bexpr EQ bexpr                              {     Eq($1, $3)                          }
  | bexpr NEQ bexpr                             {     Neq($1, $3)                         }
  | bexpr LT bexpr                              {     Lt($1, $3)                          }
  | bexpr GT bexpr                              {     Gt($1, $3)                          }
  | bexpr LTE bexpr                             {     Lte($1, $3)                         }
  | bexpr GTE bexpr                             {     Gte($1, $3)                         }
  | bexpr AND bexpr                             {     And($1, $3)                         }
  | bexpr OR bexpr                              {     Or($1, $3)                          }
  | NOT bexpr                                   {     Not($2)                             }
;

expr:
  | expr sexpr                                  {     Apply($1, $2)                       }
  | sexpr                                       {     $1                                  }
;
