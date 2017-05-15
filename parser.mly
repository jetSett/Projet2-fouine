%{
  open Lexer_type;;
%}

%token EOF
%token STP END_PROG

// expr
%token LPARENT RPARENT
%token LET IN FUN RARROW LARROW POINT AMAKE IF THEN ELSE REF DEREF SET IMP REC AFFECT COMMA
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

%left COMMA

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

%nonassoc AMAKE
%nonassoc POINT

%left STP
%left ENDPROG
%left LPARENT RPARENT

%start main

%type <Lexer_type.typed_expr> main

%%

main:
  | dexpr STP          { $1 }
  | dexpr END_PROG     { $1 }
;

variable:
  | VAR                                         {       Expression.Var($1)                           }
;

lvariable:
  |                                             {       []                                }
  | variable lvariable                          {       $1::$2                            }
;

sexpr:
  | variable                                    {       T_Variable($1)                           }
  | INT					                                {       T_Const_int($1)                          }
  | TRUE                                        {       T_Const_bool(true)                       }
  | FALSE                                       {       T_Const_bool(false)                      }
  | LPARENT expr RPARENT                        {       $2                                     }
  | LPARENT RPARENT                             {       T_Unit                                   }
;

dexpr:
  | LET REC variable lvariable EQ expr STP dexpr  {     T_Let_rec($3, Nothing_t, map_fun $4 $6, $8)      }
  | LET variable lvariable EQ expr STP dexpr      {     T_Let_in($2, Nothing_t, map_fun $3 $5, $7)       }
  | LET variable COMMA variable EQ expr STP dexpr {     T_Let_match($2, Nothing_t, $4, Nothing_t, $6, $8)           }
  | expr                                          {     $1                                  }

expr:
  | IF bexpr THEN expr ELSE expr                {     T_IfThenElse($2, $4, $6)              }
  | PRINT expr                                  {     T_PrInt($2)                           }
  | expr IMP expr                               {     T_Imp($1, $3)                         }

  | LET REC variable lvariable EQ expr IN expr    {     T_Let_rec($3, Nothing_t, map_fun $4 $6, $8)      }
  | LET variable lvariable EQ expr IN expr        {     T_Let_in($2, Nothing_t, map_fun $3 $5, $7)       }
  | LET variable COMMA variable EQ expr IN expr   {     T_Let_match($2, Nothing_t, $4, Nothing_t, $6, $8)           }
  | FUN variable lvariable RARROW expr            {     T_Function_arg($2, Nothing_t, map_fun $3 $5, Nothing_t)     }

  | expr COMMA expr                             {     T_Comma($1, $3)                       }

  | TRY expr WITH EXCEPT variable RARROW expr    {     T_TryWith($2, $5, $7)                 }
  | RAISE LPARENT EXCEPT sexpr RPARENT           {     T_Raise($4)                           }

  | REF expr                                    {     T_Reference($2)                       }
  | DEREF expr                                  {     T_Deference($2)                       }
  | variable SET expr                           {     T_Set($1, $3)                         }

  | AMAKE sexpr                                     {     T_AMake($2)                       }
  | variable POINT LPARENT expr RPARENT LARROW expr {     T_ArrayWrite($1, $4, $7)          }
  | variable POINT LPARENT expr RPARENT             {     T_ArrayAccess($1, $4)             }

  | expr PLUS expr                              {     T_Plus($1, $3)                        }
  | expr MINUS expr                             {     T_Minus($1, $3)                       }
  | expr TIMES expr                             {     T_Times($1, $3)                       }
  | expr DIVIDE expr                            {     T_Divide($1, $3)                      }
  | MINUS expr %prec UMINUS                     {     T_Minus(T_Const_int(0), $2)             }

  | INT                                         {     T_Const_int($1)                       }
  | TRUE                                        {     T_Const_bool(true)                    }
  | FALSE                                       {     T_Const_bool(false)                   }
  | variable                                    {     T_Variable($1)                        }

  | LPARENT expr RPARENT                        {     $2                                  }
  | LPARENT RPARENT                             {     T_Unit                                }

  | funct_call                                  {     $1                                  }
;

bexpr:
  | LPARENT bexpr RPARENT                     {     $2                                  }
  | expr EQ expr                              {     T_Eq($1, $3)                          }
  | expr NEQ expr                             {     T_Neq($1, $3)                         }
  | expr LT expr                              {     T_Lt($1, $3)                          }
  | expr GT expr                              {     T_Gt($1, $3)                          }
  | expr LTE expr                             {     T_Lte($1, $3)                         }
  | expr GTE expr                             {     T_Gte($1, $3)                         }
  | bexpr AND bexpr                           {     T_And($1, $3)                         }
  | bexpr OR bexpr                            {     T_Or($1, $3)                          }
  | NOT bexpr                                 {     T_Not($2)                             }
;

funct_call:
  | funct_call sexpr                            {     T_Apply($1, $2)                       }
  | sexpr sexpr                                 {     T_Apply($1, $2)                       }
;
