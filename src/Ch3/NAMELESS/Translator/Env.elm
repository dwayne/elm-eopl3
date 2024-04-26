module Ch3.NAMELESS.Translator.Env exposing
    ( Env
    , empty
    , extend
    , find
    )

--
-- The static environment.
--

import Ch3.NAMELESS.Translator.AST exposing (LexAddr)


type Env id
    = Env (List id)


empty : Env id
empty =
    Env []


extend : id -> Env id -> Env id
extend name (Env names) =
    Env (name :: names)


find : id -> Env id -> Maybe LexAddr
find needle (Env names) =
    indexOf needle names


indexOf : a -> List a -> Maybe Int
indexOf x xs =
    indexOfHelper 0 x xs


indexOfHelper : Int -> a -> List a -> Maybe Int
indexOfHelper i x xs =
    case xs of
        [] ->
            Nothing

        y :: rest ->
            if x == y then
                Just i

            else
                indexOfHelper (i + 1) x rest
