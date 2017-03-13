open Ast
open Printf
open Arg
open Parser
open Lexer
open Lexing

exception Compile_error of string

let compile_error msg = raise (Compile_error msg)

let variables = Stack.create ()

(* let functions = Hashtbl.create 10 *)

let depth = ref 0

let rec push_all l stack =
    match l with
    | [] -> ()
    | h::t -> Stack.push h stack; push_all t stack;
    | _ -> compile_error "expected a list to push onto stack"

let rec sort = function
    | [] -> []
    | x :: l -> insert x (sort l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem < x then elem :: x :: l
                else x :: insert elem l;;

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

let rec add_vars names vars stack =
    match names with
    | [] -> AstVoid()
    | h::t -> Stack.push (AstAssignment(h, List.hd vars)) stack; add_vars t (List.tl vars) stack

let get_table t stack =
    let tbl = get_var t stack in
    match tbl with
    | AstTable table -> table
    | _ -> compile_error "expected an AstTable node"

let add_entry table entry env =
    match entry with
    | AstTableEntry(AstAutoIndex(true), AstVar(v)) -> Hashtbl.add table (AstStr(v)) (get_var v env)
    | AstTableEntry(AstAutoIndex(true), value) -> Hashtbl.add table (AstInt(Hashtbl.length table)) value
    | AstTableEntry(key, value) -> Hashtbl.add table key value
    | _ -> compile_error "Invalid table entry given";;

let sort_table table = 
    let values = Hashtbl.fold (fun k v acc -> (match v with AstStr(s) -> s :: acc | AstInt(i) -> (string_of_int i) :: acc | _ -> acc)) table [] in
    let sorted_values = sort values in 
    let i = ref 0 in (
        print_int (List.length sorted_values); print_newline ();
        List.iter (fun elem -> Hashtbl.replace table (AstInt(!i)) (AstStr(elem)); i := !i + 1) sorted_values;
    )

let add_default_tbl_methods table tableName env =
    let def_count = 4 in
    let tbladd = AstFunc("add", ["key"; "value"], [AstTableAssign(tableName, AstVar("key"), AstVar("value"))]) in
    let tblappend = AstFunc("append", ["value"], [AstTableAssign(tableName, AstSub(AstTableLen(tableName), AstInt(def_count)), AstVar("value"))]) in
    let tblremove = AstFunc("remove", ["key"], [AstTableRemove(tableName, AstVar("key"))]) in 
    let tblsort = AstFunc("sort", [], [AstTableSort(tableName)]) in (
        Hashtbl.add table (AstStr("add")) tbladd;
        Hashtbl.add table (AstStr("append")) tblappend;
        Hashtbl.add table (AstStr("remove")) tblremove;
        Hashtbl.add table (AstStr("sort")) tblsort;
    )

let create_table l env =
    let tbl = Hashtbl.create 100 in
    let rec loop elements =
        match elements with
        | [] -> tbl
        | h::t -> add_entry tbl h env; loop t
        | _ -> compile_error "expected list of AstTableEntry nodes" in
    loop l;;

let rec print_type results env = 
    match results with
    | AstExpressions([]) -> ()
    | AstVoid() -> ()
    | AstInt i -> print_int i
    | AstBool(true) -> print_string "True"
    | AstBool(false) -> print_string "False"
    | AstStr s -> print_string s
    | AstVar v -> (let value = get_var v env in print_type value env)
    | AstFunc (n,_,_) -> print_string ("function " ^ n);
    | AstFuncRet (n,_,_,_) -> print_string ("function " ^ n);
    | AstTable t -> print_string "{"; Hashtbl.iter (fun k v -> print_type k env; print_string ":"; print_type v env; print_string ", ";) t; print_string "}"
    | AstTableEntry (k, v) -> print_type k env; print_string ":"; print_type v env; print_string ", ";
    | AstExpressions expressions -> List.iter (fun expr -> print_type expr env) expressions
    | _ -> compile_error "unknown type" ;;

let compile_assign assign env = 
    match assign with
    | AstAssignment(name, AstTable(t)) -> add_default_tbl_methods t name env; Stack.push assign env
    | _ -> Stack.push assign env
    
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

let rec compile_expression expression env =
    let rec compile_whileloop (cond, exprs) env =
    match (compile_expression cond env) with
    | AstBool(true) -> compile_expressions exprs env; compile_whileloop (cond, exprs) env
    | AstBool(false) -> AstVoid()
    | _ -> compile_error "Invalid condition used for loop" in

    let rec compile_forloop forloop env =
    match forloop with
    | AstForloop (decls, cond, incs, exprs) -> compile_expressions decls env; compile_whileloop (cond, List.append exprs incs) env
    | _ -> compile_error "Unknown AST node used for for-loop" in

    let compile_function_call func args env =
    let envFunc = Stack.copy env in
    match func with 
    | AstFunc (_, p, b) -> add_vars p args envFunc; compile_expressions b envFunc; AstVoid();
    | AstFuncRet (_,p,b,r) -> add_vars p args envFunc; compile_expressions b envFunc; compile_expression r envFunc;
    | _ -> compile_error "Expected an AstFunc or AstFuncRet node" in

    match expression with
    | AstInt i -> AstInt i
    | AstBool b -> AstBool b
    | AstStr s -> AstStr s
    | AstDouble d -> AstDouble d
    | AstVar v -> get_var v env
    | AstTable t -> AstTable t
    | AstTableGet(t, k) -> Hashtbl.find (get_table t env) k
    | AstTableCreate l -> AstTable(create_table l env)
    | AstTableAssign (t, k, v) -> Hashtbl.replace (get_table t env) (compile_expression k env) (compile_expression v env); AstVoid();
    | AstTableRemove (t, k) -> Hashtbl.remove (get_table t env) (compile_expression k env); AstVoid()
    | AstTableFunc (t,f,args) -> compile_function_call (Hashtbl.find (get_table t env) f) args env
    | AstTableLen (t) -> AstInt(Hashtbl.length (get_table t env))
    | AstTableSort (t) -> sort_table (get_table t env); AstVoid();
    | AstFunc (n,p,b) -> compile_assign (AstAssignment(n, AstFunc(n,p,b))) env; AstVoid()
    | AstFuncRet (n,p,b,r) -> compile_assign (AstAssignment(n, AstFuncRet(n,p,b,r))) env; AstVoid()
    | AstFuncCall (fname, args) -> compile_function_call (get_var fname env) args env
    | AstEquals (e1, e2) -> compile_logicalops (AstEquals (compile_expression e1 env, compile_expression e2 env))
    | AstLessThan (e1, e2) -> compile_logicalops (AstLessThan (compile_expression e1 env, compile_expression e2 env))
    | AstGreaterThan (e1, e2) -> compile_logicalops (AstGreaterThan (compile_expression e1 env, compile_expression e2 env))
    | AstLTEqual (e1, e2) -> compile_logicalops (AstLTEqual (compile_expression e1 env, compile_expression e2 env))
    | AstGTEqual (e1, e2) -> compile_logicalops (AstGTEqual (compile_expression e1 env, compile_expression e2 env))
    | AstAdd (e1, e2) -> compile_mathops (AstAdd (compile_expression e1 env, compile_expression e2 env))
    | AstSub (e1, e2) -> compile_mathops (AstSub (compile_expression e1 env, compile_expression e2 env))
    | AstMul (e1, e2) -> compile_mathops (AstMul (compile_expression e1 env, compile_expression e2 env))
    | AstDiv (e1, e2) -> compile_mathops (AstDiv (compile_expression e1 env, compile_expression e2 env))
    | AstMod (e1, e2) -> compile_mathops (AstMod (compile_expression e1 env, compile_expression e2 env))
    | AstNegate e -> (let res = compile_expression e env in match res with AstInt i -> AstInt(~-i) | AstDouble d -> AstDouble(~-.d) | _ -> compile_error "Invalid operation")
    | AstAssignment (vname, value) -> compile_assign (AstAssignment (vname, compile_expression value env)) env ; AstVar vname
    | AstIf (AstInt i, e2) -> compile_error "can't evaluate an integer as a boolean expression"
    | AstIf (AstBool(true), e2) -> compile_expressions e2 env
    | AstIf (AstBool(false), e2) -> AstVoid()
    | AstIf (cond, e1) -> compile_expression (AstIf (compile_expression cond env, e1)) env
    | AstIfElse (AstInt i, e2, e3) -> compile_error "can't evaluate an integer as a boolean expression"
    | AstIfElse (AstBool(true), e2, e3) -> compile_expressions e2 env
    | AstIfElse (AstBool(false), e2, e3) -> compile_expressions e3 env
    | AstIfElse (cond, e1, e2) -> compile_expression (AstIfElse (compile_expression cond env, e1, e2)) env
    | AstForloop (decl, cond, inc, exprs) -> compile_forloop (AstForloop (decl, cond, inc, exprs)) env
    | AstWhile (cond, exprs) -> compile_whileloop (cond, exprs) env
    | AstPrint (expr) -> print_type (compile_expression expr env) env; AstVoid()
    | _ -> compile_error "invalid expression";

and compile_expressions expressions env = AstExpressions(List.map (fun expr -> compile_expression expr env) expressions)

let compile p env =
    match p with
        | AstExpressions expressions -> compile_expressions expressions env
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
let _ = compile parsedProg variables in
flush stdout