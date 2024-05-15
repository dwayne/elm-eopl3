module Ch5.THREADS.AST exposing
    ( Expr(..)
    , Id
    , Number
    , Procrec
    , Program(..)
    )


type Program
    = Program Expr


type Expr
    = Const Number
    | Var Id
    | Diff Expr Expr
    | Zero Expr
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr
    | Null Expr
    | EmptyList
    | List (List Expr)
    | If Expr Expr Expr
    | Let Id Expr Expr
    | Proc Id Expr
    | Letrec (List Procrec) Expr
    | Call Expr Expr
    | Set Id Expr
    | Begin Expr (List Expr)
    | Print Expr


type alias Procrec =
    { name : Id
    , param : Id
    , body : Expr
    }


type alias Number =
    Int


type alias Id =
    String
