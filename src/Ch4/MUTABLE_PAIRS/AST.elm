module Ch4.MUTABLE_PAIRS.AST exposing
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
    | If Expr Expr Expr
    | Let Id Expr Expr
    | Proc Id Expr
    | Letrec (List Procrec) Expr
    | Call Expr Expr
    | Set Id Expr
    | Begin Expr (List Expr)
    | Newpair Expr Expr
    | Left Expr
    | Right Expr
    | Setleft Expr Expr
    | Setright Expr Expr


type alias Procrec =
    { name : Id
    , param : Id
    , body : Expr
    }


type alias Number =
    Int


type alias Id =
    String
