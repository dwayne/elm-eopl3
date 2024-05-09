module Lib.Eval exposing
    ( Eval
    , andThen
    , fail
    , followedBy
    , fromResult
    , getState
    , map
    , runEval
    , setState
    , succeed
    )


type Eval s e a
    = Eval (s -> ( Result e a, s ))


runEval : s -> Eval s e a -> Result e a
runEval state (Eval eval) =
    Tuple.first <| eval state


fromResult : Result e a -> Eval s e a
fromResult result =
    case result of
        Ok a ->
            succeed a

        Err e ->
            fail e


succeed : a -> Eval s e a
succeed a =
    Eval (\state -> ( Ok a, state ))


fail : e -> Eval s e a
fail e =
    Eval (\state -> ( Err e, state ))


getState : Eval s e s
getState =
    Eval (\state -> ( Ok state, state ))


setState : s -> Eval s e ()
setState state =
    Eval (\_ -> ( Ok (), state ))


map : (a -> b) -> Eval s e a -> Eval s e b
map f (Eval evalA) =
    Eval <|
        \state0 ->
            let
                ( resultA, state1 ) =
                    evalA state0
            in
            case resultA of
                Ok a ->
                    ( Ok <| f a, state1 )

                Err e ->
                    ( Err e, state1 )


andThen : (a -> Eval s e b) -> Eval s e a -> Eval s e b
andThen f (Eval evalA) =
    Eval <|
        \state0 ->
            let
                ( resultA, state1 ) =
                    evalA state0
            in
            case resultA of
                Ok a ->
                    let
                        (Eval evalB) =
                            f a
                    in
                    evalB state1

                Err err ->
                    ( Err err, state1 )


followedBy : Eval s e b -> Eval s e a -> Eval s e b
followedBy evalB evalA =
    evalA |> andThen (\_ -> evalB)
