module Ch5.EXCEPTIONS.Parser exposing (Error, parse)

import Ch5.EXCEPTIONS.AST as AST exposing (..)
import Lib.Lexer as L
import Parser as P exposing ((|.), (|=), Parser)
import Set


type alias Error =
    List P.DeadEnd


parse : String -> Result (List P.DeadEnd) AST.Program
parse =
    P.run program


program : Parser AST.Program
program =
    P.succeed Program
        |. L.spaces
        |= expr
        |. P.end


expr : Parser Expr
expr =
    P.oneOf
        [ diffExpr
        , zeroExpr
        , ifExpr
        , letExpr
        , procExpr
        , letrecExpr
        , callExpr
        , tryExpr
        , raiseExpr
        , constExpr
        , varExpr
        ]


diffExpr : Parser Expr
diffExpr =
    P.succeed Diff
        |. L.symbol "-"
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |. L.symbol ","
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


zeroExpr : Parser Expr
zeroExpr =
    P.succeed Zero
        |. L.keyword "zero?"
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


ifExpr : Parser Expr
ifExpr =
    P.succeed If
        |. L.keyword "if"
        |= P.lazy (\_ -> expr)
        |. L.keyword "then"
        |= P.lazy (\_ -> expr)
        |. L.keyword "else"
        |= P.lazy (\_ -> expr)


letExpr : Parser Expr
letExpr =
    P.succeed Let
        |. L.keyword "let"
        |= id
        |. L.symbol "="
        |= P.lazy (\_ -> expr)
        |. L.keyword "in"
        |= P.lazy (\_ -> expr)


procExpr : Parser Expr
procExpr =
    P.succeed Proc
        |. L.keyword "proc"
        |. L.symbol "("
        |= id
        |. L.symbol ")"
        |= P.lazy (\_ -> expr)


letrecExpr : Parser Expr
letrecExpr =
    P.succeed Letrec
        |. L.keyword "letrec"
        |= id
        |. L.symbol "("
        |= id
        |. L.symbol ")"
        |. L.symbol "="
        |= P.lazy (\_ -> expr)
        |. L.keyword "in"
        |= P.lazy (\_ -> expr)


callExpr : Parser Expr
callExpr =
    P.succeed Call
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


tryExpr : Parser Expr
tryExpr =
    P.succeed Try
        |. L.keyword "try"
        |= P.lazy (\_ -> expr)
        |. L.keyword "catch"
        |. L.symbol "("
        |= id
        |. L.symbol ")"
        |= P.lazy (\_ -> expr)


raiseExpr : Parser Expr
raiseExpr =
    P.succeed Raise
        |. L.keyword "raise"
        |= P.lazy (\_ -> expr)


constExpr : Parser Expr
constExpr =
    P.map Const number


number : Parser Number
number =
    L.digits


varExpr : Parser Expr
varExpr =
    P.map Var id


id : Parser String
id =
    L.makeIdentifier
        { start = Char.isLower
        , inner = Char.isLower
        , reserved =
            Set.fromList
                [ "else"
                , "if"
                , "in"
                , "let"
                , "letrec"
                , "proc"
                , "then"
                , "zero?"
                , "try"
                , "catch"
                , "raise"
                ]
        }
