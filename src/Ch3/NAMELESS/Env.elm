module Ch3.NAMELESS.Env exposing
    ( Env
    , empty
    , extend
    , find
    )

--
-- The NAMELESS environment.
--

import Ch3.NAMELESS.Translator.AST exposing (LexAddr)


type Env v
    = Env (List v)


empty : Env v
empty =
    Env []


extend : v -> Env v -> Env v
extend value (Env values) =
    Env (value :: values)


find : LexAddr -> Env v -> Maybe v
find lexAddr (Env values) =
    findHelper lexAddr values


findHelper : Int -> List a -> Maybe a
findHelper i xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if i == 0 then
                Just x

            else
                findHelper (i - 1) rest
