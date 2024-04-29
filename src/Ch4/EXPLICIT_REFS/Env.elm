module Ch4.EXPLICIT_REFS.Env exposing
    ( Env
    , Item(..)
    , RecBinding
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
    | RecBind (List (RecBinding id e)) (Env id v e) -- [( name, param, body )]


type alias RecBinding id e =
    { name : id
    , param : id
    , body : e
    }


empty : Env id v e
empty =
    Empty


extend : id -> v -> Env id v e -> Env id v e
extend =
    Bind


extendRec : List (RecBinding id e) -> Env id v e -> Env id v e
extendRec =
    RecBind


type Item id v e
    = Value v
    | Procedure id e (Env id v e)


find : id -> Env id v e -> Maybe (Item id v e)
find needle env =
    case env of
        Empty ->
            Nothing

        Bind name value restEnv ->
            if needle == name then
                Just <| Value value

            else
                find needle restEnv

        RecBind recBindings restEnv ->
            case findRecBinding needle recBindings of
                Just { param, body } ->
                    Just <| Procedure param body env

                Nothing ->
                    find needle restEnv


findRecBinding : id -> List (RecBinding id e) -> Maybe (RecBinding id e)
findRecBinding needle recBindings =
    case recBindings of
        [] ->
            Nothing

        recBinding :: restRecBindings ->
            if needle == recBinding.name then
                Just recBinding

            else
                findRecBinding needle restRecBindings
