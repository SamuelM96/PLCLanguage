%{
    open AST
%}

%token EOF
%token EOL
%token <string> IDENT
%token <int> LITERAL
%token ASSIGN
%token FUNCTION RETURN NULL
%token DOT
%token LINECOMMENT MULTILINECOMOPEN MULTILINECOMCLOSE
%token OPENBRACER CLOSEBRACER
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQUALS LESSTHAN GREATERTHAN LTEQUAL GTEQUAL NOT AND OR
%token INC DEC
%token WHILE FOR
%token IF IFELSE ELSE
%token COMMA SEMICOLON
%token ADDOP SUBOP MULOP DIVOP MOD
%right ASSIGN
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
%start main
%type <int> main
%%

main:
    statements EOF                      { PROGRAM $1 }
;

statements:
    { [] }
    | statement SEMICOLON statements                                                                            { $1 :: $3 }
    | FUNCTION LPAREN expression_list RPAREN OPENBRACER statements CLOSEBRACER                                  { FUNCTION ($3, $6) }
    | FOR LPAREN statement SEMICOLON statement SEMICOLON statement RPAREN OPENBRACER statements CLOSEBRACER     { FORLOOP ($3, $5, $7, $10) }
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
    | INC LITERAL                       { incr $2 }
    | DEC LITERAL                       { decr $2 }
    | expression ADDOP expression       { ADD ($1, $3) }
    | expression SUBOP expression       { SUB ($1, $3) }
    | expression MULOP expression       { MUL ($1, $3) }
    | expression DIVOP expression       { DIV ($1, $3) }
    | expression MOD expression         { MOD ($1, $3) }
    | LITERAL ADDOP LITERAL             { NUMBER ($1 + $3) }
    | LITERAL SUBOP LITERAL             { NUMBER ($1 - $3) }
    | LITERAL MULOP LITERAL             { NUMBER ($1 * $3) }
    | LITERAL DIVOP LITERAL             { NUMBER ($1 / $3) }
    | LITERAL MOD LITERAL               { NUMBER ($1 % $3) }
    | SUBOP LITERAL %prec UMINUS        { NUMBER (- $2) }
    | expression NOT expression         { BOOLEAN ($1 <> $3) }
    | expression EQUALS expression      { BOOLEAN ($1 = $3) }
    | expression AND expression         { BOOLEAN ($1 and $3) }
    | expression OR expression          { BOOLEAN ($1 or $3) }
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

%%