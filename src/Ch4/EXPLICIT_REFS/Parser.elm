module Ch4.EXPLICIT_REFS.Parser exposing (Error, parse)

import Ch4.EXPLICIT_REFS.AST as AST exposing (..)
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
        , newrefExpr
        , derefExpr
        , setrefExpr
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


newrefExpr : Parser Expr
newrefExpr =
    P.succeed Newref
        |. L.keyword "newref"
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


derefExpr : Parser Expr
derefExpr =
    P.succeed Deref
        |. L.keyword "deref"
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


setrefExpr : Parser Expr
setrefExpr =
    P.succeed Setref
        |. L.keyword "setref"
        |. L.symbol "("
        |= P.lazy (\_ -> expr)
        |. L.symbol ","
        |= P.lazy (\_ -> expr)
        |. L.symbol ")"


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
                [ "deref"
                , "else"
                , "if"
                , "in"
                , "let"
                , "letrec"
                , "newref"
                , "proc"
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
