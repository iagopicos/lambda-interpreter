type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
  | TmRapp of string * term * term 
  | TmTuple of term * term
;;

type instruction = 
    TmAssigment of string * term
  | TmEvaluation of term
;;

val string_of_term : term -> string
;;

val string_of_instruction : instruction -> string 
;; 

exception NoRuleApplies
;;

val eval : (string, term) Hashtbl.t -> term -> term
;;
