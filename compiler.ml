open Ast
open Printf
open Arg
open Parser
open Lexer
open Lexing

exception Compile_error of string

let compile_error msg = raise (Compile_error msg)

let variables = Stack.create ()

let depth = ref 0

let rec push_all l stack =
    match l with
    | [] -> ()
    | h::t -> Stack.push h stack; push_all t stack;
    | _ -> compile_error "expected a list to push onto stack"

let rec get_assignment vname stack = 
    let vars = Stack.copy stack in
    try
        let var = Stack.pop vars in
        match var with 
        | AstAssignment(vn, value) -> if vn <> vname then get_assignment vname vars else AstAssignment(vn, value) 
        | _ -> compile_error "expected an AstAssignment node";
    with Stack.Empty -> AstVoid();;

let rec get_var vname stack = 
    let assignment = get_assignment vname stack in
    match assignment with
    | AstAssignment(vn, value) -> value
    | AstVoid() -> AstVoid();
    | _ -> compile_error "Unknown assignment"

let get_table t stack =
    let tbl = get_var t stack in
    match tbl with
    | AstTable table -> table
    | _ -> compile_error "expected an AstTable node"

(* let rec get_var vname stack = 
    let vars = Stack.copy stack in
    try
        let var = Stack.pop vars in
        match var with AstAssignment(vn, value) -> if vn <> vname then get_var vname vars else value | _ -> compile_error "expected an AstAssignment node";
    with Stack.Empty -> AstVoid();; *)


(* let rec set_var vname value stack acc =
    try
        let var = Stack.pop stack in
        let () = var :: acc in
        match var with 
        AstAssignment(vn, value) -> if vn <> vname then 
                                        set_var vname value stack acc 
                                    else begin
                                        push_all (List.tl acc) stack;
                                        Stack.push AstAssignment(vn, value) stack;
                                    end
        | _ -> compile_error "expected an AstAssignment node";
    with Stack.Empty -> AstVoid();; *)


(* let get_table_value table key =
    match table with
    | AstTable t -> (try List.assoc key (List.map (fun tableEntry -> match tableEntry with AstTableEntry(k,v) -> (k,v) | _ -> compile_error "expected an AstTableEntry node") t) with Not_found -> AstVoid())
    | _ -> compile_error "Expected a AstTable node" *)

(* let set_table_value table key value =
    let new_table = 
        match table with
        | AstTable t -> (List.map (fun tableEntry -> match tableEntry with AstTableEntry(key,v) -> if k = key then (k,v) else () | _ -> compile_error "expected an AstTableEntry node") t)
        | _ -> compile_error "Expected a AstTable node" in
    let _ = set_var  *)

let add_entry table entry =
    match entry with
    | AstTableEntry(k,v) -> Hashtbl.add table k v
    | _ -> compile_error "expected AstTableEntry node"

let create_table l =
    let tbl = Hashtbl.create 100 in
    let rec loop elements =
        match elements with
        | [] -> tbl
        | h::t -> add_entry tbl h; loop t
        | _ -> compile_error "expected list of AstTableEntry nodes" in
    loop l;;

let rec print_type results = 
    match results with
    | AstExpressions([]) -> ()
    | AstVoid() -> ()
    | AstInt i -> print_int i
    | AstBool(true) -> print_string "True"
    | AstBool(false) -> print_string "False"
    | AstStr s -> print_string s
    | AstVar v -> (let value = get_var v variables in print_type value)
    | AstTable t -> print_string "{"; Hashtbl.iter (fun k v -> print_type k; print_string ":"; print_type v; print_string ", ";) t; print_string "}"
    | AstTableEntry (k, v) -> print_type k; print_string ":"; print_type v; print_string ", ";
    | AstExpressions expressions -> List.iter print_type expressions
    | _ -> compile_error "unknown type" ;;

let compile_assign assign = Stack.push assign variables
    
let compile_logicalops expr =
    match expr with
    | AstEquals (AstInt(i1), AstInt(i2)) -> AstBool (i1 = i2)
    | AstLessThan (AstInt(i1), AstInt(i2)) -> AstBool (i1 < i2)
    | AstGreaterThan (AstInt(i1), AstInt(i2)) -> AstBool (i1 > i2)
    | AstLTEqual (AstInt(i1), AstInt(i2)) -> AstBool (i1 <= i2)
    | AstGTEqual (AstInt(i1), AstInt(i2)) -> AstBool (i1 >= i2)
    | _ -> compile_error "Invalid logical operation"

let compile_mathops expr = 
    match expr with
    | AstAdd (AstStr s1, AstStr s2) -> AstStr(s1 ^ s2)
    | AstAdd (AstInt i1, AstInt i2) -> AstInt(i1 + i2)
    | AstSub (AstInt i1, AstInt i2) -> AstInt(i1 - i2)
    | AstMul (AstInt i1, AstInt i2) -> AstInt(i1 * i2)
    | AstDiv (AstInt i1, AstInt i2) -> AstInt(i1 / i2)
    | AstMod (AstInt i1, AstInt i2) -> AstInt(i1 mod i2)
    | AstAdd (AstDouble d1, AstDouble d2) -> AstDouble(d1 +. d2)
    | AstSub (AstDouble d1, AstDouble d2) -> AstDouble(d1 -. d2)
    | AstMul (AstDouble d1, AstDouble d2) -> AstDouble(d1 *. d2)
    | AstDiv (AstDouble d1, AstDouble d2) -> AstDouble(d1 /. d2)
    | _ -> compile_error "Invalid operation performed on variables"

let rec compile_expression expression =
    let rec compile_whileloop (cond, exprs) =
    match (compile_expression cond) with
    | AstBool(true) -> compile_expressions exprs; compile_whileloop (cond, exprs)
    | AstBool(false) -> AstVoid()
    | _ -> compile_error "Invalid condition used for loop" in

    let rec compile_forloop forloop =
    match forloop with
    | AstForloop (decls, cond, incs, exprs) -> compile_expressions decls; compile_whileloop (cond, List.append exprs incs)
    | _ -> compile_error "Unknown AST node used for for-loop" in

    match expression with
    | AstInt i -> AstInt i
    | AstBool b -> AstBool b
    | AstStr s -> AstStr s
    | AstDouble d -> AstDouble d
    | AstVar v -> get_var v variables
    | AstTable t -> AstTable t
    | AstTableGet(t, k) -> Hashtbl.find (get_table t variables) k
    | AstTableCreate l -> AstTable(create_table l);
    | AstTableAssign (t, k, v) -> Hashtbl.replace (get_table t variables) k v; AstVoid();
    | AstNegate e -> (let res = compile_expression e in match res with AstInt i -> AstInt(-i))
    | AstEquals (e1, e2) -> compile_logicalops (AstEquals (compile_expression e1, compile_expression e2))
    | AstLessThan (e1, e2) -> compile_logicalops (AstLessThan (compile_expression e1, compile_expression e2))
    | AstGreaterThan (e1, e2) -> compile_logicalops (AstGreaterThan (compile_expression e1, compile_expression e2))
    | AstLTEqual (e1, e2) -> compile_logicalops (AstLTEqual (compile_expression e1, compile_expression e2))
    | AstGTEqual (e1, e2) -> compile_logicalops (AstGTEqual (compile_expression e1, compile_expression e2))
    | AstAdd (e1, e2) -> compile_mathops (AstAdd (compile_expression e1, compile_expression e2))
    | AstSub (e1, e2) -> compile_mathops (AstSub (compile_expression e1, compile_expression e2))
    | AstMul (e1, e2) -> compile_mathops (AstMul (compile_expression e1, compile_expression e2))
    | AstDiv (e1, e2) -> compile_mathops (AstDiv (compile_expression e1, compile_expression e2))
    | AstMod (e1, e2) -> compile_mathops (AstMod (compile_expression e1, compile_expression e2))
    | AstAssignment (vname, value) -> compile_assign (AstAssignment (vname, compile_expression value)) ; AstVar vname
    | AstIf (AstInt i, e2) -> compile_error "can't evaluate an integer as a boolean expression"
    | AstIf (AstBool(true), e2) -> compile_expressions e2
    | AstIf (AstBool(false), e2) -> AstVoid()
    | AstIf (cond, e1) -> compile_expression (AstIf (compile_expression cond, e1))
    | AstIfElse (AstInt i, e2, e3) -> compile_error "can't evaluate an integer as a boolean expression"
    | AstIfElse (AstBool(true), e2, e3) -> compile_expressions e2
    | AstIfElse (AstBool(false), e2, e3) -> compile_expressions e3
    | AstIfElse (cond, e1, e2) -> compile_expression (AstIfElse (compile_expression cond, e1, e2))
    | AstForloop (decl, cond, inc, exprs) -> compile_forloop (AstForloop (decl, cond, inc, exprs))
    | AstWhile (cond, exprs) -> compile_whileloop (cond, exprs)
    | AstPrint (expr) -> print_type (compile_expression expr) ; AstVoid()
    | _ -> compile_error "invalid expression";

and compile_expressions expressions = AstExpressions(List.map compile_expression expressions)

let compile p =
    match p with
        | AstExpressions expressions -> compile_expressions expressions
        | _ -> compile_error "expected expressions"

let parseProgram c = 
    let lexbuf = Lexing.from_channel c in
    try
        parser_main lexer_main lexbuf 
    with Parsing.Parse_error -> failwith (compile_error ("Parse failure! " ^ lexbuf.lex_curr_p.pos_fname));;

let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./compiler PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !arg in
let _ = compile parsedProg in
(* let () = print_type results in *)
flush stdout