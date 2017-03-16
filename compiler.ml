open Ast
open Printf
open Arg
open Parser
open Lexer
open Lexing

exception Compile_error of string
exception Break_stmt of unit

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
        List.iter (fun elem -> Hashtbl.replace table (AstInt(!i)) (AstStr(elem)); i := !i + 1) sorted_values;
    )

let add_default_tbl_methods table tableName env =
    let tbladd = AstFunc("add", ["key"; "value"], [AstTableAssign(tableName, AstVar("key"), AstVar("value"))]) in
    let tblappend = AstFunc("append", ["value"], [AstTableAssign(tableName, AstLen(tableName), AstVar("value"))]) in
    let tblremove = AstFunc("remove", ["key"], [AstTableRemove(tableName, AstVar("key"))]) in 
    let tblsort = AstFunc("sort", [], [AstTableSort(tableName)]) in (
        Hashtbl.add table (AstStr("add")) tbladd;
        Hashtbl.add table (AstStr("append")) tblappend;
        Hashtbl.add table (AstStr("remove")) tblremove;
        Hashtbl.add table (AstStr("sort")) tblsort;
    )

let rec var_to_string v env =
    match v with
    | AstExpressions([]) -> ""
    | AstVoid() -> ""
    | AstInt i -> string_of_int i
    | AstDouble d -> string_of_float d
    | AstBool(true) -> "True"
    | AstBool(false) -> "False"
    | AstStr s -> s
    | AstVar v -> (let value = get_var v env in var_to_string value env)
    | AstFunc (n,_,_) -> "function " ^ n
    | AstFuncRet (n,_,_,_) -> "function " ^ n
    | AstTable t -> "{" ^ (Hashtbl.fold (fun k v acc -> match v with AstFunc (_,_,_) -> acc | AstFuncRet(_,_,_,_) -> acc | _ -> (acc ^ (var_to_string k env) ^ ":" ^ (var_to_string v env) ^ ", ")) t "") ^ "}"
    | AstTableEntry (k, v) -> (var_to_string k env) ^ ":" ^ (var_to_string v env) ^ ", "
    | _ -> compile_error "Unknown type or incompatible type" ;;

let rec print_type v env = print_string (var_to_string v env)

let create_table l env =
    let tbl = Hashtbl.create 100 in
    let rec loop elements =
        match elements with
        | [] -> tbl
        | h::t -> add_entry tbl h env; loop t
        | _ -> compile_error "expected list of AstTableEntry nodes" in
    loop l;;

let compile_assign assign env = 
    match assign with
    | AstAssignment(name, AstTable(t)) -> add_default_tbl_methods t name env; Stack.push assign env
    | _ -> Stack.push assign env
    
let compile_logicalops expr =
    match expr with
    | AstEquals (e1, e2) -> AstBool (e1 = e2)
    | AstNotEquals (e1, e2) -> AstBool (e1 <> e2)
    | AstAnd (AstBool(e1), AstBool(e2)) -> AstBool (e1 && e2)
    | AstOr (AstBool(e1), AstBool(e2)) -> AstBool (e1 || e2)
    | AstNot (AstBool(e)) -> AstBool (not e)
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
    try
        match (compile_expression cond env) with
        | AstBool(true) -> compile_expressions exprs env; compile_whileloop (cond, exprs) env
        | AstBool(false) -> AstVoid()
        | _ -> compile_error "Invalid condition used for loop" ;
    with Break_stmt() -> AstVoid() in


    let rec compile_forloop forloop env =
    match forloop with
    | AstForloop (decls, cond, incs, exprs) -> compile_expressions decls env; compile_whileloop (cond, List.append exprs incs) env
    | _ -> compile_error "Unknown AST node used for for-loop" in

    let compile_function_call func args env =
    let envFunc = Stack.copy env in
    match func with 
    | AstFunc (_, p, b) -> add_vars p (List.map (fun elem -> compile_expression elem envFunc) args) envFunc; compile_expressions b envFunc; AstVoid();
    | AstFuncRet (_,p,b,r) -> add_vars p (List.map (fun elem -> compile_expression elem envFunc) args) envFunc; compile_expressions b envFunc; compile_expression r envFunc;
    | _ -> compile_error "Expected an AstFunc or AstFuncRet node" in

    let compile_read filename env =
        let ic = open_in filename in
        let fileData = ref "" in
        try
            while true do
                fileData := !fileData ^ (input_line ic) ^ "\n"
            done; !fileData
        with End_of_file -> close_in ic; !fileData in 
    
    let rec compile_write filename data env =
        let oc = open_out filename in
        match data with 
        | AstVoid() -> close_out oc
        | AstStr s -> fprintf oc "%s\n" s ; close_out oc
        | AstExpressions([]) -> close_out oc
        | AstInt i -> fprintf oc "%d\n" i ; close_out oc
        | AstDouble d -> fprintf oc "%f\n" d ; close_out oc
        | AstBool(true) -> fprintf oc "True\n" ; close_out oc
        | AstBool(false) -> fprintf oc "False\n" ; close_out oc
        | AstVar v -> (let value = get_var v env in compile_write filename value env)
        | AstFunc (n,_,_) -> fprintf oc "%s\n" ("function " ^ n) ; close_out oc
        | AstFuncRet (n,_,_,_) -> print_string ("function " ^ n);
        | AstTable t -> fprintf oc "{"; Hashtbl.iter (fun k v -> compile_write filename k env; fprintf oc ":"; compile_write filename v env; fprintf oc ", ") t; fprintf oc "}"; close_out oc
        | AstTableEntry (k, v) -> compile_write filename k env; fprintf oc ":"; compile_write filename v env; fprintf oc ","; close_out oc;
        | AstExpressions expressions -> List.iter (fun expr -> compile_write filename expr env) expressions 
        | _ -> compile_error ("Unknown type trying to be written to a file " ^ filename) in

    let compile_input env = 
        let ic = stdin in
        try
            let input = input_line ic in
            (* close_in ic; *)
            try AstInt(int_of_string input) with Failure e -> (try AstDouble(float_of_string input) with Failure e -> (try AstBool(bool_of_string (String.lowercase input)) with Invalid_argument e -> AstStr(input)))
        with e -> 
            (* close_in ic;  *)
            raise e in

    let index_string s num = 
        match num with
        | AstInt n -> String.get s n
        | _ -> compile_error "Indexing a string requires an int as the key" in


    match expression with
    | AstInt i -> AstInt i
    | AstBool b -> AstBool b
    | AstStr s -> AstStr s
    | AstDouble d -> AstDouble d
    | AstVar v -> get_var v env
    | AstTable t -> AstTable t
    | AstStrToInt s -> AstInt(int_of_string (String.trim s))
    | AstStrToBool s -> AstBool (bool_of_string (String.lowercase (String.trim s)))
    | AstStrToDouble s -> AstDouble(float_of_string (String.trim s))
    | AstVarStrToInt v -> AstInt(int_of_string (match get_var v env with AstStr s -> (String.trim s) | _ -> compile_error "Cannot convert non-string types"))
    | AstVarStrToBool v -> AstBool (bool_of_string (match get_var v env with AstStr s -> (String.lowercase (String.trim s)) | _ -> compile_error "Cannot convert non-string types"))
    | AstVarStrToDouble v -> AstDouble(float_of_string (match get_var v env with AstStr s -> (String.trim s) | _ -> compile_error "Cannot convert non-string types"))
    | AstVarToStr v -> AstStr(var_to_string (get_var v env) env)
    | AstIndexVar(t, k) -> (match (get_var t env) with AstTable table -> (try Hashtbl.find table (compile_expression k env) with Not_found -> compile_error "Out of bounds/Invalid key used") | AstStr s -> AstStr(String.make 1 (index_string s (compile_expression k env))) | _ -> compile_error "Cannot index this type")
    | AstIndexStr(s, k) -> AstStr(String.make 1 (index_string s (compile_expression k env)))
    | AstTableCreate l -> AstTable(create_table l env)
    | AstTableAssign (t, k, v) -> Hashtbl.replace (get_table t env) (compile_expression k env) (compile_expression v env); AstVoid();
    | AstTableRemove (t, k) -> Hashtbl.remove (get_table t env) (compile_expression k env); AstVoid()
    | AstTableFunc (t,f,args) -> compile_function_call (Hashtbl.find (get_table t env) f) args env
    | AstLen t -> (match (get_var t env) with AstTable table -> (AstInt(List.length (Hashtbl.fold (fun k v acc -> match v with AstFunc(_,_,_) -> acc | AstFuncRet(_,_,_,_) -> acc | _ -> k :: acc) table []))) | AstStr s -> AstInt(String.length s) | _ -> compile_error "Cannot perform length operator on this type")
    | AstStrLen s -> AstInt(String.length s)
    | AstTableSort (t) -> sort_table (get_table t env); AstVoid();
    | AstFunc (n,p,b) -> compile_assign (AstAssignment(n, AstFunc(n,p,b))) env; AstVoid()
    | AstFuncRet (n,p,b,r) -> compile_assign (AstAssignment(n, AstFuncRet(n,p,b,r))) env; AstVoid()
    | AstFuncCall (fname, args) -> compile_function_call (get_var fname env) args env
    | AstEquals (e1, e2) -> compile_logicalops (AstEquals (compile_expression e1 env, compile_expression e2 env))
    | AstNotEquals (e1, e2) -> compile_logicalops(AstNotEquals (compile_expression e1 env, compile_expression e2 env))
    | AstAnd (e1, e2) -> compile_logicalops(AstAnd (compile_expression e1 env, compile_expression e2 env))
    | AstOr (e1, e2) -> compile_logicalops(AstOr (compile_expression e1 env, compile_expression e2 env))
    | AstNot (e) -> compile_logicalops(AstNot (compile_expression e env))
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
    | AstGlobalAssignment (vname, value) -> compile_assign (AstAssignment (vname, compile_expression value env)) env ; compile_assign (AstAssignment (vname, compile_expression value env)) variables ; AstVar vname
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
    | AstDoWhile (cond, exprs) -> compile_expressions exprs env ; compile_whileloop (cond, exprs) env
    | AstPrint (expr) -> print_type (compile_expression expr env) env; AstVoid()
    | AstPrintln (expr) -> print_type (compile_expression expr env) env; print_newline () ; AstVoid()
    | AstRead (filename) -> AstStr(compile_read filename env)
    | AstReadVar (filename) -> AstStr(compile_read (match (get_var filename env) with AstStr s -> s | _ -> compile_error "Expected an AstStr node") env)
    | AstWrite (filename, data) -> compile_write filename data env; AstVoid(); 
    | AstWriteVar (filename, data) -> compile_write (match (get_var filename env) with AstStr s -> s | _ -> compile_error "Expected an AstStr node") data env; AstVoid(); 
    | AstInput () -> compile_input env
    | AstBreak() -> raise (Break_stmt ())
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