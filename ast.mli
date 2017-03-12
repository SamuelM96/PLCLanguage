type ast =
    | AstProgram of ast
    | AstAssignment of string * ast
    | AstFunction of ast list * ast
    | AstForloop of ast * ast * ast * ast
    | AstWhileLoop of ast * ast
    | AstIf of ast * ast
    | AstIfElse of ast * ast * ast
    | AstInt of int
    | AstDouble of float
    | AstBool of bool
    | AstVar of string
    | AstAdd of ast * ast
    | AstSub of ast * ast
    | AstMul of ast * ast
    | AstDiv of ast * ast
    | AstMod of ast * ast