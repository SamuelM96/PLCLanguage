%token EOL
%start <int> main
%type <int> main
%%

main:
    expr EOL                { $1 }
;

expr:

;