module Ch3.NAMELESS.Translator.AST exposing
    ( Expr(..)
    , LexAddr
    , Number
    , Program(..)
    )


type Program
    = Program Expr


type Expr
    = Const Number
    | Var LexAddr
    | Diff Expr Expr
    | Zero Expr
    | If Expr Expr Expr
    | Let Expr Expr
    | Proc Expr
    | Call Expr Expr


type alias Number =
    Int


type alias LexAddr =
    Int
