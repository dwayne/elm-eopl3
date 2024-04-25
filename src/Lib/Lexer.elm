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
    --
    -- Don't use P.int.
    --
    -- Why? Read https://github.com/elm/parser/issues/44#issuecomment-1116857918.
    --
    -- I ran into this problem once again when parsing
    --
    -- """
    -- ...
    -- (((oddmaker oddmaker) evenmaker) -(x, 1))
    -- ...
    -- """
    --
    -- Because "evenmaker" starts with an "e" it is parsed unsuccessfully by P.int.
    --
    chompOneOrMore Char.isDigit
        |> P.getChompedString
        |> P.map (Maybe.withDefault 0 << String.toInt)
        |> lexeme


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore isGood =
    P.succeed ()
        |. P.chompIf isGood
        |. P.chompWhile isGood


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
