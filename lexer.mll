{
    open Parser
    exception Eof

let line_num = ref 1

let keywords = [
    "function", FUNCTION; "return", RETURN; "while", WHILE; "if", IF; "else", ELSE; "for", FOR; "true", TRUE; "false", FALSE; "print", PRINT;
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

rule lexer_main = parse
    | '='      { ASSIGN }
    | '+'      { ADDOP }
    | '-'      { SUBOP }
    | '*'      { MULOP }
    | '/'      { DIVOP }
    | '%'      { MODOP }
    | ','      { COMMA }
    | ';'      { SEMICOLON }
    | ':'      { COLON }
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
    | '.'      { DOT }
    | '#'      { TABLELEN }
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
    | '"' { Buffer.contents buffer }
    | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
    | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
    | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
    | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
    | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
    | eof { syntax_error "end of file reached inside string" }
    | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }