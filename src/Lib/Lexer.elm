module Lib.Lexer exposing
    ( digits
    , keyword
    , makeIdentifier
    , spaces
    , symbol
    )

import Parser as P exposing ((|.), (|=), Parser)
import Set exposing (Set)


makeIdentifier :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set String
    }
    -> Parser String
makeIdentifier =
    lexeme << P.variable


digits : Parser Int
digits =
    lexeme P.int


keyword : String -> Parser ()
keyword =
    lexeme << P.keyword


symbol : String -> Parser ()
symbol =
    lexeme << P.symbol


lexeme : Parser a -> Parser a
lexeme p =
    P.succeed identity
        |= p
        |. spaces


spaces : Parser ()
spaces =
    P.spaces
