module Ch3.LETREC.Env exposing
    ( Env
    , apply
    , empty
    , extend
    )


type Env k v
    = Env (List ( k, v ))


empty : Env k v
empty =
    Env []


extend : k -> v -> Env k v -> Env k v
extend k v (Env bindings) =
    Env (( k, v ) :: bindings)


apply : k -> Env k v -> Maybe v
apply k (Env bindings) =
    lookup k bindings


lookup : k -> List ( k, v ) -> Maybe v
lookup needle assoc =
    case assoc of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if needle == k then
                Just v

            else
                lookup needle rest
