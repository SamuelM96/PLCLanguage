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

%token EOF
%token EOL
%token <string> IDENT
%token <int> INTEGER
%token <float> DOUBLE
%token <string> STRING
%token PRINT
%token ASSIGN
%token FUNCTION RETURN NULL
%token DOT
%token COLON
%token LINECOMMENT MULTILINECOMOPEN MULTILINECOMCLOSE
%token OPENBRACER CLOSEBRACER
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQUALS LESSTHAN GREATERTHAN LTEQUAL GTEQUAL NOT AND OR
%token INC DEC
%token WHILE FOR
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
%left EQUALS
%left LESSTHAN GREATERTHAN LTEQUAL GTEQUAL
%left ADDOP SUBOP
%left MULOP DIVOP MOD
%right NOT 
%nonassoc UMINUS 
%left INC DEC
%left DOT
%start parser_main
%type <Ast.ast> parser_main
%%

parser_main:
    expressions EOF  { AstExpressions $1 }
    | error { parse_error "expressions" ; AstError "Error in expressions" }
;

expressions:
    | { [] }
    | SEMICOLON expressions                                                                                                 { $2 }
    | expression SEMICOLON expressions                                                                                      { $1 :: $3 }
    | assignment SEMICOLON expressions                                                                                      { $1 :: $3 }
    | IF LPAREN IDENT RPAREN OPENBRACER expressions CLOSEBRACER expressions %prec IFX                                       { (AstIf (AstVar $3, $6)) :: $8}
    | IF LPAREN IDENT RPAREN OPENBRACER expressions CLOSEBRACER ELSE OPENBRACER expressions CLOSEBRACER expressions         { (AstIfElse (AstVar $3, $6, $10)) :: $12 }
    | IF LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER expressions %prec IFX                          { (AstIf ($3, $6)) :: $8 }
    | IF LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER ELSE OPENBRACER expressions CLOSEBRACER expressions    { (AstIfElse ($3, $6, $10)) :: $12 }
    | FOR LPAREN assignment_list SEMICOLON boolean_expression SEMICOLON assignment_list RPAREN OPENBRACER expressions CLOSEBRACER expressions { (AstForloop ($3, $5, $7, $10)) :: $12 }
    | WHILE LPAREN boolean_expression RPAREN OPENBRACER expressions CLOSEBRACER expressions                                 { (AstWhile ($3, $6)) :: $8 }
    | PRINT LPAREN expression RPAREN SEMICOLON expressions                                                                  { (AstPrint $3) :: $6 }
;

expression_list:
    | { [] }
    | expression COMMA expression_list      { $1 :: $3 }
    | assignment COMMA expression_list      { $1 :: $3 }
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
;

expression:
    | types                                 { $1 }
    | boolean_expression                    { $1 }
    | SUBOP expression %prec UMINUS         { AstNegate $2 }
    | expression SUBOP expression           { AstSub ($1, $3) }
    | expression MULOP expression           { AstMul ($1, $3) }
    | expression DIVOP expression           { AstDiv ($1, $3) }
    | expression MODOP expression           { AstMod ($1, $3) }
    | IDENT LSQUARE types RSQUARE           { AstTableGet($1, $3) }
    | LPAREN expression RPAREN              { $2 }
;

types:
    | INTEGER   { AstInt $1 }
    | DOUBLE    { AstDouble $1 }
    | TRUE      { AstBool true }
    | FALSE     { AstBool false }
    | STRING    { AstStr $1 }
    | IDENT     { AstVar $1 }
    | OPENBRACER table_decl CLOSEBRACER { AstTableCreate($2) }
;

assignment_list:
    | { [] }
    | assignment { [$1] }
    | assignment COMMA assignment_list { $1 :: $3 }
;

assignment:
    | IDENT ASSIGN expression                           { AstAssignment ($1, $3) }
    | IDENT INC                                         { AstAssignment ($1, AstAdd(AstVar($1), AstInt(1))) }
    | IDENT DEC                                         { AstAssignment ($1, AstSub(AstVar($1), AstInt(1))) }
    | IDENT LSQUARE types RSQUARE ASSIGN expression     { AstTableAssign ($1, $3, $6) }
;

table_decl:
    | { [] }
   /* | expression                                 { [AstTableEntry(AstAutoIndex(true), $1)] }
    | expression COMMA table_decl                { (AstTableEntry(AstAutoIndex(true), $1)) :: $3 } */
    | types COLON expression                     { [AstTableEntry($1, $3)] }
    | types COLON expression COMMA table_decl    { (AstTableEntry($1, $3)) :: $5 }
;

/* Not working */
/*
main:
    statements EOF                      { PROGRAM $1 }
;

statements:
    { [] }
    | statement SEMICOLON statements                                                                            { $1 :: $3 }
    | FUNCTION LPAREN expression_list RPAREN OPENBRACER statements CLOSEBRACER                                  { FUNCTION ($3, $6) }
    | FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN OPENBRACER statements CLOSEBRACER     { FORLOOP ($3, $5, $7, $10) }
    | WHILE LPAREN expression RPAREN OPENBRACER statements CLOSEBRACER                                          { WHILELOOP ($3, $6) }
    | if_statement                                                                                              { $1 }
    | LINECOMMENT                                                                                               {   }
    | MULTILINECOMOPEN statements MULTILINECOMCLOSE                                                             {   }
;

statement:
    IDENT ASSIGN expression             { ASSIGNMENT ($1, $3) } 
;

if_statement:
    | IF LPAREN expression RPAREN OPENBRACER statements CLOSEBRACER                                             { IFSTMT ($3, $6) }
    | IF LPAREN expression RPAREN OPENBRACER statements CLOSEBRACER ELSE OPENBRACER statements CLOSEBRACER      { IFELSESTMT ($3, $6, $10)}
    | IF LPAREN expression RPAREN OPENBRACER statements CLOSEBRACER ELSE if_statement                           { IFELSESTMT ($3, $6, $9)}
;

expression_list:
    { [] } 
    | expression                        { [$1] }
    | expression COMMA expression_list  { $1 :: $3 }
;

expression:
    indet_list                          { $1 }
    | IDENT                             { VAR $1 }
    | LITERAL                           { NUMBER $1 }
    | INC LITERAL                       { NUMBER (incr $2) }
    | DEC LITERAL                       { NUMBER (decr $2) }
    | expression ADDOP expression       { ADD ($1, $3) }
    | expression SUBOP expression       { SUB ($1, $3) }
    | expression MULOP expression       { MUL ($1, $3) }
    | expression DIVOP expression       { DIV ($1, $3) }
    | expression MODOP expression       { MOD ($1, $3) }
    | LITERAL ADDOP LITERAL             { NUMBER ($1 + $3) }
    | LITERAL SUBOP LITERAL             { NUMBER ($1 - $3) }
    | LITERAL MULOP LITERAL             { NUMBER ($1 * $3) }
    | LITERAL DIVOP LITERAL             { NUMBER ($1 / $3) }
    | LITERAL MODOP LITERAL             { NUMBER ($1 % $3) }
    | SUBOP LITERAL %prec UMINUS        { NUMBER (- $2) }
    | expression NOT expression         { BOOLEAN ($1 <> $3) }
    | expression EQUALS expression      { BOOLEAN ($1 = $3) }
    | expression AND expression         { BOOLEAN ($1 && $3) }
    | expression OR expression          { BOOLEAN ($1 || $3) }
    | expression LESSTHAN expression    { BOOLEAN ($1 < $3) }
    | expression GREATERTHAN expression { BOOLEAN ($1 > $3) }
    | expression LTEQUAL expression     { BOOLEAN ($1 <= $3) }
    | expression GTEQUAL expression     { BOOLEAN ($1 >= $3) }
    | LPAREN expression RPAREN          { $2 }
;

indet_list:
    IDENT                               { [$1] }
    | IDENT COMMA indet_list            { $1 :: $3 }
;

*/
%%