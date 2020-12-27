
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let context = Hashtbl.create 12345
;;

let execute_ctx ctx inst = match inst with
  TmEvaluation tm -> 
    print_endline( string_of_term(eval ctx tm) );
     ctx
  | TmAssigment(key, value) -> 
    print_endline(string_of_term (eval ctx value)); 
    Hashtbl.add ctx key value ; ctx
;;


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop context =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_line ())) in
      let ctx = execute_ctx context tm in 
        loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop context
     | Parse_error ->
         print_endline "syntax error";
         loop context
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop context
  ;;

top_level_loop ()
;;

