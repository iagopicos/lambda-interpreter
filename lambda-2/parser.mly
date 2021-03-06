
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN

%token LPAREN
%token RPAREN
%token DOT
%token COMA
%token EQ
%token EOF

%token <int> INTV
%token <string> STRINGV

%start s
%type <Lambda.instruction> s

%%

s :
    instruction EOF
      { $1 }

instruction: 
    STRINGV EQ term 
        { TmAssigment ($1,$3) }
    | term 
        { TmEvaluation $1 }
        
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV DOT term
      { TmAbs ($2, $4) }
  | LET STRINGV EQ term IN term
      { TmApp (TmAbs ($2, $6), $4) }
  | LETREC STRINGV EQ term IN term
      { TmRapp ($2,$4, $6) } 
  | appTerm COMA appTerm
     {TmTuple ($1, $3) }
appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }