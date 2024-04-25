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
    | Var Id
    | Diff Expr Expr
    | Zero Expr
    | If Expr Expr Expr
    | Let Id Expr Expr


type alias Number =
    Int


type alias Id =
    String
