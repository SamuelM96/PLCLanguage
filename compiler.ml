open Ast

exception Compile_error of string

let compile_error msg = raise (Compile_error msg)

let variables = Hashbl.create 100

let compile_assign name val = Hashbl.add name val

let compile_expression expression =
    match expression with
    | NUMBER i -> i
    | BOOLEAN b -> b
    | VAR v -> try Hashbl.find variables v
                with Not_found -> compile_error "uninitialised variable being used"
    | ADD (e1, e2) -> (compile_expression e1) + (compile_expression e2)
    | SUB (e1, e2) -> (compile_expression e1) - (compile_expression e2)
    | MUL (e1, e2) -> (compile_expression e1) * (compile_expression e2)
    | DIV (e1, e2) -> (compile_expression e1) / (compile_expression e2)
    | MOD (e1, e2) -> (compile_expression e1) % (compile_expression e2)

let compile_expressions expressions = List.iter compile_expression

let compile_statement statement = 
    match statement with
    | ASSIGNMENT (name, val) -> compile_assign name (compile_expressions val)
    | FUNCTION (args, statements) -> compile_function (compile_expressions args) (compile_statements statements)
    | FORLOOP (init, condition, increment, statements) -> compile_forloop (compile_statement init) (compile_expression condition) (compile_statement increment) (compile_statements statements)
    | WHILELOOP (condition, statements) -> compile_whileloop (compile_expression condition) (compile_statements statements)
    | IFSTMT (condition, statements) -> compile_ifstmt (compile_expression condition) (compile_statements statements)
    | IFELSESTMT (condition, ifstatements, elsestatements) -> compile_ifelsestmt (compile_expression condition) (compile_statements ifstatements) (compile_statements elsestatements)
    | _ -> compile_error "expected statement type AST node"

let compile_statements statements = List.iter compile_statement

let compiler p =
    match p with
        | PROGRAM statements -> Printf.printf (compile_statements statements)
        | _ -> compile_error "expected program AST node"