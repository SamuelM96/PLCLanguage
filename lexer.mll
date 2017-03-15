{
    open Parser
    exception Eof

let line_num = ref 1

let keywords = [
    "function", FUNCTION; "return", RETURN; "break", BREAK; "while", WHILE; "do", DO;
    "if", IF; "else", ELSE; "for", FOR; "true", TRUE; "false", FALSE; "print", PRINT; 
    "println", PRINTLN; "read", READ; "write", WRITE; "input", INPUT; "null", NULL; 
    "global", GLOBAL; "string_to_int", STRINGTOINT; "string_to_bool", STRINGTOBOOL;
    "string_to_double", STRINGTODOUBLE; "var_to_string", VARTOSTRING;
]

exception Syntax_error of string

let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))

}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let digits = digit*
let frac = '.' digits
let float = digits* frac?
let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*
let string = "\"(\\.|[^\"])*\""
let singleLineComment = "//"_*['\n' '\r']*
let multilineComment = "/*"_*"*/"

rule lexer_main = parse
    | '='       { ASSIGN }
    | '+'       { ADDOP }
    | '-'       { SUBOP }
    | '*'       { MULOP }
    | '/'       { DIVOP }
    | '%'       { MODOP }
    | "+="      { ADDASSIGN }
    | "-="      { SUBASSIGN }
    | "*="      { MULASSIGN }
    | "/="      { DIVASSIGN }
    | "%="      { MODASSIGN }
    | ','       { COMMA }
    | ';'       { SEMICOLON }
    | ':'       { COLON }
    | '('       { LPAREN }
    | ')'       { RPAREN }
    | '{'       { OPENBRACER }
    | '}'       { CLOSEBRACER }
    | singleLineComment     { LINECOMMENT }
    | multilineComment      { MULTILINECOMMENT }
    | "++"      { INC }
    | "--"      { DEC }
    | "&&"      { AND }
    | "||"      { OR }
    | '!'       { NOT }
    | '<'       { LESSTHAN }
    | '>'       { GREATERTHAN }
    | "<="      { LTEQUAL }
    | ">="      { GTEQUAL }
    | "=="      { EQUALS }
    | "!="      { NOTEQUALS }
    | '['       { LSQUARE }
    | ']'       { RSQUARE }
    | '.'       { DOT }
    | '#'       { LEN }
    | blank     { lexer_main lexbuf }
    | iden as i {
        let l = String.lowercase i in
        try List.assoc l keywords
        with Not_found -> IDENT i   
    }
    | digits as d {
        INTEGER (int_of_string d)
    }
    | float as f {
        DOUBLE (float_of_string f)
    }
    | '\n'      { incr line_num; lexer_main lexbuf }
    | eof       { EOF }
    | '"' { let buffer = Buffer.create 1 in STRING (stringl buffer lexbuf) }
    | _         { syntax_error ("couldn't identify token " ^ (Lexing.lexeme lexbuf)) }
    and  stringl buffer = parse
    | '"'       { Buffer.contents buffer }
    | "\\t"     { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
    | "\\n"     { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
    | "\\n"     { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
    | '\\' '"'  { Buffer.add_char buffer '"'; stringl buffer lexbuf }
    | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
    | eof       { syntax_error "end of file reached inside string" }
    | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }