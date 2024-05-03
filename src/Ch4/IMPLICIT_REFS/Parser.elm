module Ch4.IMPLICIT_REFS.Parser exposing (Error, parse)

import Ch4.IMPLICIT_REFS.AST as AST exposing (..)
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
        , setExpr
        , beginExpr
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
        |= many procrec
        |. L.keyword "in"
        |= P.lazy (\_ -> expr)


procrec : Parser Procrec
procrec =
    P.succeed Procrec
        |= id
        |. L.symbol "("
        |= id
        |. L.symbol ")"
        |. L.symbol "="
        |= P.lazy (\_ -> expr)


callExpr : Parser Expr
callExpr =
    P.succeed Call
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


setExpr : Parser Expr
setExpr =
    P.succeed Set
        |. L.keyword "set"
        |= id
        |. L.symbol "="
        |= P.lazy (\_ -> expr)


beginExpr : Parser Expr
beginExpr =
    P.succeed Begin
        |. L.keyword "begin"
        |= P.lazy (\_ -> expr)
        |= many
            (P.succeed identity
                |. L.symbol ";"
                |= P.lazy (\_ -> expr)
            )
        |. L.keyword "end"


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
                [ "begin"
                , "else"
                , "end"
                , "if"
                , "in"
                , "let"
                , "letrec"
                , "proc"
                , "set"
                , "then"
                , "zero?"
                ]
        }



-- HELPERS


many : Parser a -> Parser (List a)
many p =
    P.loop [] <|
        \rev ->
            P.oneOf
                [ P.map (\x -> P.Loop (x :: rev)) p
                , P.succeed (P.Done (List.reverse rev))
                ]
