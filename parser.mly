%{
    open Ast
    open Lexing

    exception ParseErr of string

    let error msg start finish  = 
        Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum 
              (start.pos_cnum -start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg

    let parse_error msg =
        raise ( ParseErr (error msg (rhs_start_pos 1) (rhs_end_pos 1)))
%}

%token EOF EOFTYPE
%token EOL
%token <string> IDENT
%token <int> INTEGER
%token <float> DOUBLE
%token <string> STRING
%token NULL 
%token GLOBAL
%token PRINT PRINTLN READ WRITE INPUT
%token ASSIGN ADDASSIGN SUBASSIGN MULASSIGN DIVASSIGN MODASSIGN
%token STRINGTOINT STRINGTOBOOL STRINGTODOUBLE VARTOSTRING
%token FUNCTION RETURN BREAK
%token DOT LEN
%token COLON
%token LINECOMMENT MULTILINECOMMENT
%token OPENBRACER CLOSEBRACER
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQUALS NOTEQUALS LESSTHAN GREATERTHAN LTEQUAL GTEQUAL NOT AND OR
%token INC DEC
%token DO WHILE FOR
%token IF IFELSE ELSE
%token TRUE FALSE
%token COMMA SEMICOLON
%token ADDOP SUBOP MULOP DIVOP MODOP
%right ASSIGN
%nonassoc IFX 
%nonassoc IF ELSE OPENBRACER CLOSEBRACER
%nonassoc LPAREN RPAREN
%left OR
%left AND
%left EQUALS NOTEQUALS
%left LESSTHAN GREATERTHAN LTEQUAL GTEQUAL
%left ADDOP SUBOP
%left MULOP DIVOP MOD
%right NOT 
%nonassoc UMINUS 
%left INC DEC
%right LEN
%left DOT
%start parser_main
%type <Ast.ast> parser_main
%%

parser_main:
    expressions EOF  { AstExpressions $1 }
    | error { parse_error "Expected expressions" }
;

expressions:
    | { [] }
    | SEMICOLON expressions                                                                                                 { $2 }
    | expression SEMICOLON expressions                                                                                      { $1 :: $3 }
    | assignment SEMICOLON expressions                                                                                      { $1 :: $3 }
    | if_statements expressions                                                                                             { $1 :: $2 }
    | FOR LPAREN assignment_list SEMICOLON boolean_expression SEMICOLON assignment_list RPAREN OPENBRACER expressions CLOSEBRACER expressions { (AstForloop ($3, $5, $7, $10)) :: $12 }
    | DO OPENBRACER expressions CLOSEBRACER WHILE LPAREN boolean_expression RPAREN SEMICOLON expressions                    { (AstDoWhile ($7, $3)) :: $10 }
    | WHILE LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER expressions                                 { (AstWhile ($3, $6)) :: $8 }
    | PRINT LPAREN expression RPAREN SEMICOLON expressions                                                                  { (AstPrint $3) :: $6 }
    | PRINTLN LPAREN expression RPAREN SEMICOLON expressions                                                                { (AstPrintln $3) :: $6 }
    | FUNCTION IDENT LPAREN ident_list RPAREN OPENBRACER expressions CLOSEBRACER expressions                                { (AstFunc ($2, $4, $7)) :: $9}
    | FUNCTION IDENT LPAREN ident_list RPAREN OPENBRACER expressions RETURN expression SEMICOLON CLOSEBRACER expressions    { (AstFuncRet ($2, $4, $7, $9)) :: $12}
    | LINECOMMENT expressions                                                                                               { $2 }
    | MULTILINECOMMENT expressions                                                                                          { $2 }
    | error { parse_error "Expected expressions" }
;

if_statements:
    | IF LPAREN IDENT RPAREN OPENBRACER expressions CLOSEBRACER %prec IFX                                       { AstIf (AstVar $3, $6) }
    | IF LPAREN IDENT RPAREN OPENBRACER expressions CLOSEBRACER ELSE OPENBRACER expressions CLOSEBRACER         { AstIfElse (AstVar $3, $6, $10) }
    | IF LPAREN IDENT RPAREN OPENBRACER expressions CLOSEBRACER ELSE if_statements                              { AstIfElse (AstVar $3, $6, [$9]) }
    | IF LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER %prec IFX                          { AstIf ($3, $6) }
    | IF LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER ELSE OPENBRACER expressions CLOSEBRACER    { AstIfElse ($3, $6, $10) }
    | IF LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER ELSE if_statements                 { AstIfElse ($3, $6, [$9]) }
    | error { parse_error "Expected expressions" }
;

expression_list:
    | { [] }
    | expression                            { [$1] }
    | assignment                            { [$1] }
    | expression COMMA expression_list      { $1 :: $3 }
    | assignment COMMA expression_list      { $1 :: $3 }
    | error { parse_error "Expected expressions" }
;

boolean_expression:
    | TRUE                                  { AstBool true }
    | FALSE                                 { AstBool false }
    | expression EQUALS expression          { AstEquals ($1, $3) }
    | expression LESSTHAN expression        { AstLessThan ($1, $3) }
    | expression GREATERTHAN expression     { AstGreaterThan ($1, $3) }
    | expression LTEQUAL expression         { AstLTEqual ($1, $3) }
    | expression GTEQUAL expression         { AstGTEqual ($1, $3) }
    | expression ADDOP expression           { AstAdd ($1, $3) }
    | expression EQUALS expression          { AstEquals($1, $3) }
    | expression NOTEQUALS expression       { AstNotEquals($1, $3) }
    | expression AND expression             { AstAnd($1, $3) }
    | expression OR expression              { AstOr($1, $3) }
    | NOT expression                        { AstNot($2) }
    | error { parse_error "Expected expressions" }
;

expression:
    | types                                 { $1 }
    | boolean_expression                    { $1 }
    | SUBOP expression %prec UMINUS         { AstNegate $2 }
    | expression SUBOP expression           { AstSub ($1, $3) }
    | expression MULOP expression           { AstMul ($1, $3) }
    | expression DIVOP expression           { AstDiv ($1, $3) }
    | expression MODOP expression           { AstMod ($1, $3) }
    | STRINGTOINT LPAREN STRING RPAREN      { AstStrToInt $3 }
    | STRINGTOBOOL LPAREN STRING RPAREN     { AstStrToBool $3 }
    | STRINGTODOUBLE LPAREN STRING RPAREN   { AstStrToDouble $3 }
    | STRINGTOINT LPAREN IDENT RPAREN       { AstVarStrToInt $3 }
    | STRINGTOBOOL LPAREN IDENT RPAREN      { AstVarStrToBool $3 }
    | STRINGTODOUBLE LPAREN IDENT RPAREN    { AstVarStrToDouble $3 }
    | VARTOSTRING LPAREN IDENT RPAREN       { AstVarToStr $3 }
    | IDENT LSQUARE types RSQUARE           { AstIndexVar($1, $3) }
    | STRING LSQUARE types RSQUARE          { AstIndexStr($1, $3) }
    | IDENT LPAREN expression_list RPAREN   { AstFuncCall($1, $3) }
    | LPAREN expression RPAREN              { $2 }
    | IDENT DOT IDENT LPAREN params RPAREN  { AstTableFunc($1, AstStr($3), $5) }
    | LEN IDENT                             { AstLen($2) }
    | LEN STRING                            { AstStrLen($2) }
    | BREAK                                 { AstBreak() }
    | READ LPAREN STRING RPAREN             { AstRead $3 }
    | READ LPAREN IDENT RPAREN              { AstReadVar $3 }
    | WRITE LPAREN STRING COMMA expression RPAREN { AstWrite ($3, $5) }
    | WRITE LPAREN IDENT COMMA expression RPAREN  { AstWriteVar ($3, $5) }
    | INPUT LPAREN RPAREN                   { AstInput() }
    | error { parse_error "Expected expressions" }
;

types:
    | INTEGER   { AstInt $1 }
    | DOUBLE    { AstDouble $1 }
    | TRUE      { AstBool true }
    | FALSE     { AstBool false }
    | STRING    { AstStr $1 }
    | IDENT     { AstVar $1 }
    | NULL      { AstVoid() }
    | EOFTYPE   { AstEOF() }
    | OPENBRACER table_decl CLOSEBRACER { AstTableCreate($2) }
    | error { parse_error "Expected expressions" }
;

params:
    | {[]}
    | types                     { [$1] }
    | expression                { [$1] }
    | types COMMA params        { $1 :: $3 }
    | expression COMMA params   { $1 :: $3 }
    | error { parse_error "Expected expressions" }
;

ident_list:
    | { [] }
    | IDENT                     { [$1] }
    | IDENT COMMA ident_list    { $1 :: $3 }
    | error { parse_error "Expected expressions" }
;

assignment_list:
    | { [] }
    | assignment { [$1] }
    | assignment COMMA assignment_list { $1 :: $3 }
    | error { parse_error "Expected expressions" }
;

assignment:
    | IDENT ASSIGN expression                           { AstAssignment ($1, $3) }
    | GLOBAL IDENT ASSIGN expression                    { AstGlobalAssignment ($2, $4) }
    | IDENT ADDASSIGN expression                        { AstAssignment ($1, AstAdd(AstVar($1), $3)) }
    | IDENT SUBASSIGN expression                        { AstAssignment ($1, AstSub(AstVar($1), $3)) }
    | IDENT MULASSIGN expression                        { AstAssignment ($1, AstMul(AstVar($1), $3)) }
    | IDENT DIVASSIGN expression                        { AstAssignment ($1, AstDiv(AstVar($1), $3)) }
    | IDENT MODASSIGN expression                        { AstAssignment ($1, AstMod(AstVar($1), $3)) }
    | IDENT INC                                         { AstAssignment ($1, AstAdd(AstVar($1), AstInt(1))) }
    | IDENT DEC                                         { AstAssignment ($1, AstSub(AstVar($1), AstInt(1))) }
    | IDENT LSQUARE types RSQUARE ASSIGN expression     { AstTableAssign ($1, $3, $6) }
    | error { parse_error "Expected expressions" }
;

table_decl:
    | { [] }
    | expression                                 { [AstTableEntry(AstAutoIndex(true), $1)] }
    | expression COMMA table_decl                { (AstTableEntry(AstAutoIndex(true), $1)) :: $3 }
    | types COLON expression                     { [AstTableEntry($1, $3)] }
    | types COLON expression COMMA table_decl    { (AstTableEntry($1, $3)) :: $5 }
    | error { parse_error "Expected expressions" }
;

%%