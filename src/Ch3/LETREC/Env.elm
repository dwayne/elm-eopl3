module Ch3.LETREC.Env exposing
    ( Env
    , Found(..)
    , empty
    , extend
    , extendRec
    , find
    )


type
    Env id v e
    --  ^  ^ ^
    --  |  | |
    --  |  | +--- the type for expressions
    --  |  +----- the type for values
    --  +-------- the type for identifiers
    = Empty
    | Bind id v (Env id v e) -- name value
    | RecBind id id e (Env id v e) -- name param body


empty : Env id v e
empty =
    Empty


extend : id -> v -> Env id v e -> Env id v e
extend =
    Bind


extendRec : id -> id -> e -> Env id v e -> Env id v e
extendRec =
    RecBind


type Found id v e
    = Value v
    | Procedure id e (Env id v e)


find : id -> Env id v e -> Maybe (Found id v e)
find needle env =
    case env of
        Empty ->
            Nothing

        Bind name value nextEnv ->
            if needle == name then
                Just <| Value value

            else
                find needle nextEnv

        RecBind name param body nextEnv ->
            if needle == name then
                Just <| Procedure param body env

            else
                find needle nextEnv
