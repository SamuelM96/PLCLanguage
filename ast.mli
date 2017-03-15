type ast =
    | AstExpressions of ast list
    | AstVoid of unit
    | AstAssignment of string * ast
    | AstGlobalAssignment of string * ast
    | AstFunction of ast list * ast
    | AstForloop of ast list * ast * ast list * ast list
    | AstWhile of ast * ast list
    | AstDoWhile of ast * ast list
    | AstIf of ast * ast list
    | AstIfElse of ast * ast list * ast list
    | AstEquals of ast * ast
    | AstNotEquals of ast * ast
    | AstAnd of ast * ast
    | AstOr of ast * ast
    | AstNot of ast
    | AstLessThan of ast * ast
    | AstGreaterThan of ast * ast
    | AstLTEqual of ast * ast
    | AstGTEqual of ast * ast
    | AstInt of int
    | AstDouble of float
    | AstBool of bool
    | AstStr of string
    | AstStrToInt of string
    | AstStrToBool of string
    | AstStrToDouble of string
    | AstVarStrToInt of string
    | AstVarStrToBool of string
    | AstVarStrToDouble of string
    | AstVarToStr of string
    | AstTable of (ast, ast) Hashtbl.t
    | AstTableCreate of ast list
    | AstTableEntry of ast * ast
    | AstTableAssign of string * ast * ast
    | AstTableRemove of string * ast
    | AstTableFunc of string * ast * ast list
    | AstTableSort of string
    | AstAutoIndex of bool
    | AstIndexVar of string * ast
    | AstIndexStr of string * ast
    | AstLen of string
    | AstStrLen of string
    | AstVar of string
    | AstAdd of ast * ast
    | AstSub of ast * ast
    | AstNegate of ast
    | AstMul of ast * ast
    | AstDiv of ast * ast
    | AstMod of ast * ast
    | AstPrint of ast
    | AstPrintln of ast
    | AstRead of string
    | AstWrite of string * ast
    | AstReadVar of string
    | AstWriteVar of string * ast
    | AstInput of unit
    | AstFunc of string * string list * ast list
    | AstFuncCall of string * ast list
    | AstFuncRet of string * string list * ast list * ast
    | AstBreak of unit