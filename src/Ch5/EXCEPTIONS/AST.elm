module Ch5.EXCEPTIONS.AST exposing
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
    | Proc Id Expr
    | Letrec Id Id Expr Expr
    | Call Expr Expr
    | Try Expr Id Expr
    | Raise Expr


type alias Number =
    Int


type alias Id =
    String
