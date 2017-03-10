type ast =
    | PROGRAM of ast list
    | ASSIGNMENT of string * ast
    | FUNCTION of ast list * ast
    | FORLOOP of ast * ast * ast * ast
    | WHILELOOP of ast * ast
    | IFSTMT of ast * ast
    | IFELSESTMT of ast * ast * ast
    | NUMBER of int
    | BOOLEAN of bool
    | VAR of string
    | ADD of ast * ast
    | SUB of ast * ast
    | MUL of ast * ast
    | DIV of ast * ast
    | MOD of ast * ast