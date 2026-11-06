module Ch3.PROC.AST exposing
    ( Expr(..)
    , Id
    , Number
    , Program(..)
    )


type Program
    = Program Expr


type Expr
    = Const Number
    | Diff Expr Expr
    | Zero Expr
    | If Expr Expr Expr
    | Var Id
    | Let Id Expr Expr
    | Proc Id Expr
    | Call Expr Expr


type alias Number =
    Int


type alias Id =
    String
