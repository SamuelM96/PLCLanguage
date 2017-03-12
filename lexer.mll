{
    open Parser
    exception Eof

let line_num = ref 1

let keywords = [
    "function", FUNCTION; "while", WHILE; "if", IF; "else", ELSE; "for", FOR; "true", TRUE; "false", FALSE;
]

exception Syntax_error of string

let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))

}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let digits = digit*
let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*

rule lexer_main = parse
    | '='      { ASSIGN }
    | '+'      { ADDOP }
    | '-'      { SUBOP }
    | '*'      { MULOP }
    | '/'      { DIVOP }
    | '%'      { MODOP }
    | ','      { COMMA }
    | ';'      { SEMICOLON }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | '{'      { OPENBRACER }
    | '}'      { CLOSEBRACER }
    | "//"     { LINECOMMENT }
    | "/*"     { MULTILINECOMOPEN }
    | "*/"     { MULTILINECOMCLOSE }
    | "++"     { INC }
    | "--"     { DEC }
    | "&&"     { AND }
    | "||"     { OR }
    | '!'      { NOT }
    | '<'      { LESSTHAN }
    | '>'      { GREATERTHAN }
    | "<="     { LTEQUAL }
    | ">="     { GTEQUAL }
    | "=="     { EQUALS }
    | '['      { LSQUARE }
    | ']'      { RSQUARE }
    | blank     { lexer_main lexbuf }
    | iden as i {
        let l = String.lowercase i in
        try List.assoc l keywords
        with Not_found -> IDENT i   
    }
    | digits as d {
        INTEGER (int_of_string d)
    }
    | '\n'      { incr line_num; lexer_main lexbuf }
    | _         { syntax_error "couldn't identify token" }
    | eof       { EOF }