open Ast
open Printf
open Arg
open Parser
open Lexer

exception Compile_error of string

let compile_error msg = raise (Compile_error msg)

(* let variables = Hashtbl.create 100 *)

(* let compile_assign name v = Hashtbl.add variables name v *)

let compile_condition cond = 
    match cond with
    | AstBool b -> b
    | _ -> compile_error "expected a boolean type"

let rec compile_expression expression =
    match expression with
    | AstInt i -> i
    | AstIf (AstInt i, e2) -> compile_error "can't evaluate an integer as a boolean expression"
    | AstIf (AstBool(true), e2) -> compile_expression e2
    | AstIf (AstBool(false), e2) -> 0
    | AstIf (cond, e1) -> (if (compile_condition cond) then (compile_expression e1) else 0)
    | AstIfElse (AstInt i, e2, e3) -> compile_error "can't evaluate an integer as a boolean expression"
    | AstIfElse (AstBool(true), e2, e3) -> compile_expression e2
    | AstIfElse (AstBool(false), e2, e3) -> compile_expression e3
    | AstIfElse (cond, e1, e2) -> (if (compile_condition cond) then (compile_expression e1) else (compile_expression e2)) 
    (* | VAR v -> (try Hashtbl.find variables v
                with Not_found -> compile_error "uninitialised variable being used") *)
    | _ -> compile_error "invalid expression token"

let compiler p =
    match p with
        | AstProgram expression -> compile_expression expression
        | _ -> compile_error "expected program AST node"

let parseProgram c = 
    try let lexbuf = Lexing.from_channel c in  
            parser_main lexer_main lexbuf 
    with Parsing.Parse_error -> failwith "Parse failure!" ;;


let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./compiler PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !arg in
print_int (compile_expression parsedProg);
flush stdout