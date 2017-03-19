open Ast
open Printf
open Arg
open Parser
open Lexer
open Lexing

exception Evaluate_error of string
exception Break_stmt of unit

let evaluate_error msg = raise (Evaluate_error msg)

let variables = Stack.create ()

(* Pushes everything from list l onto the given stack *)
let rec push_all l stack =
    match l with
    | [] -> ()
    | h::t -> Stack.push h stack; push_all t stack

(* Basic insertion sort taken from the OCaml tutorials *)
let rec sort = function
    | [] -> []
    | x :: l -> insert x (sort l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem < x then elem :: x :: l
                else x :: insert elem l;;

(* Gets the current assignment to identifier vname on the given stack *)
let rec get_assignment vname stack = 
    let vars = Stack.copy stack in
    try
        let var = Stack.pop vars in
        match var with 
        | AstAssignment(vn, value) -> if vn <> vname then get_assignment vname vars else AstAssignment(vn, value) 
        | _ -> evaluate_error "Expected an AstAssignment node!";
    with Stack.Empty -> AstVoid();;

(* Gets the value of the given variable from the given environment *)
let rec get_var vname env = 
    let assignment = get_assignment vname env in
    match assignment with
    | AstAssignment(vn, value) -> value
    | AstVoid() -> AstVoid();
    | _ -> evaluate_error "Unknown assignment. (This shouldn't happen!)"

(* Creates assignments from the variables names in names to the values in vars and adds them to the given environment *)
let rec add_vars names vars env =
    match names with
    | [] -> ()
    | h::t -> try (Stack.push (AstAssignment(h, List.hd vars)) env; add_vars t (List.tl vars) env) with Failure e -> evaluate_error "Expected more arguments"

(* Gets the Hash table linked to the given variable name *)
let get_table t stack =
    let tbl = get_var t stack in
    match tbl with
    | AstTable table -> table
    | _ -> evaluate_error "This variable isn't of type Table."

(* Gets the length of the given Hash table. It ignores any functions stored in the tables when counting *)
let table_length table = List.length (Hashtbl.fold (fun k v acc -> match v with AstFunc(_,_,_) -> acc | AstFuncRet(_,_,_,_) -> acc | _ -> k :: acc) table [])

(* Removes the entry with the given key from the given Hash table. If the key is an integer, all entries furher in the list will be 'moved' down to fill the space *)
let remove_entry t key env = 
match key with
| AstInt i -> Hashtbl.remove t key; (let length = table_length t in for index = i to length do ( try Hashtbl.replace t (AstInt(index)) (Hashtbl.find t (AstInt(index + 1))) with Not_found -> Hashtbl.remove t (AstInt index)) done)
| _ -> Hashtbl.remove t key

(* Adds the given entry to the given Hash table *)
let add_entry table entry env =
    match entry with
    | AstTableEntry(AstAutoIndex(true), AstVar(v)) -> (let variable = (get_var v env) in match variable with AstFunc(n,_,_) -> Hashtbl.add table (AstStr(n)) variable | AstFuncRet(n,_,_,_) -> Hashtbl.add table (AstStr(n)) variable | _ -> Hashtbl.add table (AstInt(Hashtbl.length table)) variable)
    | AstTableEntry(AstAutoIndex(true), value) -> Hashtbl.add table (AstInt(Hashtbl.length table)) value
    | AstTableEntry(key, value) -> Hashtbl.add table key value
    | _ -> evaluate_error "Invalid table entry given";;

(* Sorts the given Hash table via lexographic ordering, if the values are ints or strings. Other value types are ignored. *)
let sort_table table = 
    let values = Hashtbl.fold (fun k v acc -> (match v with AstStr(s) -> s :: acc | AstInt(i) -> (string_of_int i) :: acc | _ -> acc)) table [] in
    let sorted_values = sort values in 
    let i = ref 0 in (
        List.iter (fun elem -> Hashtbl.replace table (AstInt(!i)) (AstStr(elem)); i := !i + 1) sorted_values;
    )

(* Adds the default methods to the given table variable. 
    add() allows a new key:value pair to be added to the table. 
    append() allows a value to be appened onto the end of the table by giving it an automatic index.
    remove() removes a key from the table.
    sort() sorts the table in lexographic ordering.
*)
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

(* Converts the basic Ast types to strings *)
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
    | _ -> evaluate_error "Unknown type or incompatible type attempted to be converted to a string" ;;

(* Prints the given Ast node *)
let rec print_type v env = print_string (var_to_string v env)

(* Creates a table from the given list of AstTableEntry objects *)
let create_table l env =
    let tbl = Hashtbl.create 100 in
    let rec loop elements =
        match elements with
        | [] -> tbl
        | h::t -> add_entry tbl h env; loop t in
    loop l;;

(* Evaluates the given AstAssignment node by adding it to the current environment *)
let eval_assign assign env = 
    match assign with
    | AstAssignment(name, AstTable(t)) -> add_default_tbl_methods t name env; Stack.push assign env
    | _ -> Stack.push assign env

(* Evaluates the given mathematical operation *)
let eval_mathops expr = 
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
    | _ -> evaluate_error "Invalid mathematical operation performed on variables"

(* Evaluates the given expression with the current environment *)
let rec eval_expression expression env =
    (* Evaluates the given logical operation *)
    let eval_logicalops expr =
    match expr with
    | AstEquals (e1, e2) -> AstBool (e1 = e2)
    | AstNotEquals (e1, e2) -> AstBool (e1 <> e2)
    | AstAnd (AstBool(e1), AstBool(e2)) -> AstBool (e1 && e2)
    | AstOr (AstBool(e1), AstBool(e2)) -> AstBool (e1 || e2)
    | AstNot (AstBool(e)) -> AstBool (not e)
    | AstNot e -> AstBool(not (match (eval_expression e env) with AstBool b -> b | _ -> evaluate_error "Expected boolean expression for Not(!) operation"))
    | AstLessThan (AstInt(i1), AstInt(i2)) -> AstBool (i1 < i2)
    | AstGreaterThan (AstInt(i1), AstInt(i2)) -> AstBool (i1 > i2)
    | AstLTEqual (AstInt(i1), AstInt(i2)) -> AstBool (i1 <= i2)
    | AstGTEqual (AstInt(i1), AstInt(i2)) -> AstBool (i1 >= i2)
    | _ -> evaluate_error "Invalid logical operation" in

    (* Evaluates the given AstWhile node as a while loop *)
    let rec eval_whileloop (cond, exprs) env = 
    try
        match (eval_expression cond env) with
        | AstBool(true) -> let _ = eval_expressions exprs env in eval_whileloop (cond, exprs) env
        | AstBool(false) -> AstVoid()
        | _ -> evaluate_error "Invalid condition used for loop, must evaluate to true or false" ;
    with Break_stmt() -> AstVoid() in

    (* Evaluates the given AstForLoop node as a for loop *)
    let rec eval_forloop forloop env =
    match forloop with
    | AstForloop (decls, cond, incs, exprs) -> let _ = eval_expressions decls env in eval_whileloop (cond, List.append exprs incs) env
    | _ -> evaluate_error "Unknown AST node used for for-loop. (Shouldn't be raised)" in

    (* Evaluates the given function call *)
    let eval_function_call func args env =
    let envFunc = Stack.copy env in
    match func with 
    | AstFunc (_, p, b) -> add_vars p (List.map (fun elem -> eval_expression elem envFunc) args) envFunc; let _ = eval_expressions b envFunc in AstVoid();
    | AstFuncRet (_,p,b,r) -> add_vars p (List.map (fun elem -> eval_expression elem envFunc) args) envFunc; let _ = eval_expressions b envFunc in eval_expression r envFunc;
    | _ -> evaluate_error "Expected a function for function call. This variable is likely not a declared function" in

    (* Evaluates the given AstRead node. Reads the data from the given file and returns it as a string *)
    let eval_read filename env =
        let ic = open_in filename in
        let fileData = ref "" in
        try
            while true do
                fileData := !fileData ^ (input_line ic) ^ "\n"
            done; !fileData
        with End_of_file -> close_in ic; !fileData in 
    
    (* Evaluates the given AstWrite node. Writes the given data to the given filename in the node *)
    let rec eval_write filename data env =
        let oc = open_out filename in
        match data with 
        | AstVoid() -> close_out oc
        | AstStr s -> fprintf oc "%s\n" s ; close_out oc
        | AstExpressions([]) -> close_out oc
        | AstInt i -> fprintf oc "%d\n" i ; close_out oc
        | AstDouble d -> fprintf oc "%f\n" d ; close_out oc
        | AstBool(true) -> fprintf oc "True\n" ; close_out oc
        | AstBool(false) -> fprintf oc "False\n" ; close_out oc
        | AstVar v -> (let value = get_var v env in eval_write filename value env)
        | AstFunc (n,_,_) -> fprintf oc "%s\n" ("function " ^ n) ; close_out oc
        | AstFuncRet (n,_,_,_) -> print_string ("function " ^ n);
        | AstTable t -> fprintf oc "{"; Hashtbl.iter (fun k v -> eval_write filename k env; fprintf oc ":"; eval_write filename v env; fprintf oc ", ") t; fprintf oc "}"; close_out oc
        | AstTableEntry (k, v) -> eval_write filename k env; fprintf oc ":"; eval_write filename v env; fprintf oc ","; close_out oc;
        | AstExpressions expressions -> List.iter (fun expr -> eval_write filename expr env) expressions 
        | _ -> evaluate_error ("Unknown type trying to be written to file " ^ filename) in

    (* Evaluates the given AstInput node. Reads one line from stdin *)
    let eval_input env = 
        let ic = stdin in
        try
            let input = input_line ic in
            try AstInt(int_of_string input) with Failure e -> (try AstDouble(float_of_string input) with Failure e -> (try AstBool(bool_of_string (String.lowercase input)) with Invalid_argument e -> AstStr(input)))
        with End_of_file -> 
            AstEOF() in

    (* Gets the character as the given position in the AstStr *)
    let index_string s num = 
        match num with
        | AstInt n -> String.get s n
        | _ -> evaluate_error "Indexing a string requires an int as the key" in

    (* Matches all possible Ast nodes *)
    match expression with
    | AstVoid() -> AstVoid()
    | AstEOF() -> AstEOF()
    | AstInt i -> AstInt i
    | AstBool b -> AstBool b
    | AstStr s -> AstStr s
    | AstDouble d -> AstDouble d
    | AstVar v -> get_var v env
    | AstTable t -> AstTable t
    | AstStrToInt s -> AstInt(int_of_string (String.trim s))
    | AstStrToBool s -> AstBool (bool_of_string (String.lowercase (String.trim s)))
    | AstStrToDouble s -> AstDouble(float_of_string (String.trim s))
    | AstVarStrToInt v -> AstInt(int_of_string (match get_var v env with AstStr s -> (String.trim s) | _ -> evaluate_error "Cannot convert non-string types to int"))
    | AstVarStrToBool v -> AstBool (bool_of_string (match get_var v env with AstStr s -> (String.lowercase (String.trim s)) | _ -> evaluate_error "Cannot convert non-string types to bool"))
    | AstVarStrToDouble v -> AstDouble(float_of_string (match get_var v env with AstStr s -> (String.trim s) | _ -> evaluate_error "Cannot convert non-string types to double"))
    | AstVarToStr v -> AstStr(var_to_string (get_var v env) env)
    | AstIndexVar(t, k) -> (match (get_var t env) with AstTable table -> (try Hashtbl.find table (eval_expression k env) with Not_found -> AstVoid()) | AstStr s -> AstStr(String.make 1 (index_string s (eval_expression k env))) | _ -> evaluate_error "Cannot index this type")
    | AstIndexStr(s, k) -> AstStr(String.make 1 (index_string s (eval_expression k env)))
    | AstTableCreate l -> AstTable(create_table l env)
    | AstTableAssign (t, k, v) -> Hashtbl.replace (get_table t env) (eval_expression k env) (eval_expression v env); AstVoid();
    | AstTableRemove (t, k) -> remove_entry (get_table t env) (eval_expression k env) env; AstVoid()
    | AstTableFunc (t,f,args) -> eval_function_call (Hashtbl.find (get_table t env) f) args env
    | AstLen t -> (match (get_var t env) with AstTable table -> (AstInt(table_length table)) | AstStr s -> AstInt(String.length s) | _ -> evaluate_error "Cannot perform length operator on this type")
    | AstStrLen s -> AstInt(String.length s)
    | AstTableSort (t) -> sort_table (get_table t env); AstVoid();
    | AstFunc (n,p,b) -> eval_assign (AstAssignment(n, AstFunc(n,p,b))) env; AstVoid()
    | AstFuncRet (n,p,b,r) -> eval_assign (AstAssignment(n, AstFuncRet(n,p,b,r))) env; AstVoid()
    | AstFuncCall (fname, args) -> eval_function_call (get_var fname env) args env
    | AstEquals (e1, e2) -> eval_logicalops (AstEquals (eval_expression e1 env, eval_expression e2 env))
    | AstNotEquals (e1, e2) -> eval_logicalops(AstNotEquals (eval_expression e1 env, eval_expression e2 env))
    | AstAnd (e1, e2) -> eval_logicalops(AstAnd (eval_expression e1 env, eval_expression e2 env))
    | AstOr (e1, e2) -> eval_logicalops(AstOr (eval_expression e1 env, eval_expression e2 env))
    | AstNot (e) -> eval_logicalops(AstNot (eval_expression e env))
    | AstLessThan (e1, e2) -> eval_logicalops (AstLessThan (eval_expression e1 env, eval_expression e2 env))
    | AstGreaterThan (e1, e2) -> eval_logicalops (AstGreaterThan (eval_expression e1 env, eval_expression e2 env))
    | AstLTEqual (e1, e2) -> eval_logicalops (AstLTEqual (eval_expression e1 env, eval_expression e2 env))
    | AstGTEqual (e1, e2) -> eval_logicalops (AstGTEqual (eval_expression e1 env, eval_expression e2 env))
    | AstAdd (e1, e2) -> eval_mathops (AstAdd (eval_expression e1 env, eval_expression e2 env))
    | AstSub (e1, e2) -> eval_mathops (AstSub (eval_expression e1 env, eval_expression e2 env))
    | AstMul (e1, e2) -> eval_mathops (AstMul (eval_expression e1 env, eval_expression e2 env))
    | AstDiv (e1, e2) -> eval_mathops (AstDiv (eval_expression e1 env, eval_expression e2 env))
    | AstMod (e1, e2) -> eval_mathops (AstMod (eval_expression e1 env, eval_expression e2 env))
    | AstNegate e -> (let res = eval_expression e env in match res with AstInt i -> AstInt(~-i) | AstDouble d -> AstDouble(~-.d) | _ -> evaluate_error "Invalid negation operation, only works on ints or doubles")
    | AstAssignment (vname, value) -> eval_assign (AstAssignment (vname, eval_expression value env)) env ; AstVar vname
    | AstGlobalAssignment (vname, value) -> eval_assign (AstAssignment (vname, eval_expression value env)) env ; eval_assign (AstAssignment (vname, eval_expression value env)) variables ; AstVar vname
    | AstIf (AstInt i, e2) -> evaluate_error "Can't evaluate an integer as a boolean expression"
    | AstIf (AstBool(true), e2) -> eval_expressions e2 env
    | AstIf (AstBool(false), e2) -> AstVoid()
    | AstIf (cond, e1) -> eval_expression (AstIf (eval_expression cond env, e1)) env
    | AstIfElse (AstInt i, e2, e3) -> evaluate_error "Can't evaluate an integer as a boolean expression"
    | AstIfElse (AstBool(true), e2, e3) -> eval_expressions e2 env
    | AstIfElse (AstBool(false), e2, e3) -> eval_expressions e3 env
    | AstIfElse (cond, e1, e2) -> eval_expression (AstIfElse (eval_expression cond env, e1, e2)) env
    | AstForloop (decl, cond, inc, exprs) -> eval_forloop (AstForloop (decl, cond, inc, exprs)) env
    | AstWhile (cond, exprs) -> eval_whileloop (cond, exprs) env
    | AstDoWhile (cond, exprs) -> let _ = eval_expressions exprs env in eval_whileloop (cond, exprs) env
    | AstPrint (expr) -> print_type (eval_expression expr env) env; AstVoid()
    | AstPrintln (expr) -> print_type (eval_expression expr env) env; print_newline () ; AstVoid()
    | AstRead (filename) -> AstStr(eval_read filename env)
    | AstReadVar (filename) -> AstStr(eval_read (match (get_var filename env) with AstStr s -> s | _ -> evaluate_error "Expected a string as the filename") env)
    | AstWrite (filename, data) -> eval_write filename data env; AstVoid(); 
    | AstWriteVar (filename, data) -> eval_write (match (get_var filename env) with AstStr s -> s | _ -> evaluate_error "Expected a string as the filename") data env; AstVoid(); 
    | AstInput () -> eval_input env
    | AstBreak() -> raise (Break_stmt ())
    | _ -> evaluate_error "Invalid expression";

(* Evaluates a given list of expressions and returns a list of results *)
and eval_expressions expressions env = AstExpressions(List.map (fun expr -> eval_expression expr env) expressions)

(* Evaluates the given program as a list of expressions and returns all the results *)
let evaluate p env =
    match p with
        | AstExpressions expressions -> eval_expressions expressions env
        | _ -> evaluate_error "Expected expressions"

(* Parses the given program *)
let parseProgram c = 
    let lexbuf = Lexing.from_channel c in
    try
        parser_main lexer_main lexbuf 
    with Parsing.Parse_error -> failwith (evaluate_error ("Parse failure! " ^ lexbuf.lex_curr_p.pos_fname));;

(* Basic setup to evaluate the given program file from the command line *)
let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./mysplinterpreter PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !arg in
let _ = evaluate parsedProg variables in
flush stdout