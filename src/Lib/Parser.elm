module Lib.Parser exposing (id, many, optional)

import Lib.Lexer as L
import Parser as P exposing (Parser)
import Set


id : List String -> Parser String
id keywords =
    L.makeIdentifier
        { start = Char.isLower
        , inner = Char.isLower
        , reserved = Set.fromList keywords
        }


optional : Parser a -> Parser (Maybe a)
optional p =
    P.oneOf
        [ P.map Just p
        , P.succeed Nothing
        ]


many : Parser a -> Parser (List a)
many p =
    P.loop [] <|
        \rev ->
            P.oneOf
                [ P.map (\x -> P.Loop (x :: rev)) p
                , P.succeed (P.Done (List.reverse rev))
                ]
