module Ch3.LET.Env exposing
    ( Env
    , empty
    , extend
    , find
    )


type Env id v
    = Env (List ( id, v ))


empty : Env id v
empty =
    Env []


extend : id -> v -> Env id v -> Env id v
extend name value (Env bindings) =
    Env (( name, value ) :: bindings)


find : id -> Env id v -> Maybe v
find name (Env bindings) =
    lookup name bindings


lookup : id -> List ( id, v ) -> Maybe v
lookup needle assoc =
    case assoc of
        [] ->
            Nothing

        ( name, value ) :: rest ->
            if needle == name then
                Just value

            else
                lookup needle rest
