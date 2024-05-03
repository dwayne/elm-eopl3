module Ch4.IMPLICIT_REFS.Env exposing
    ( Env
    , Item(..)
    , RecBinding
    , empty
    , extend
    , extendRec
    , find
    )

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref, Store)


type
    Env id e
    --  ^  ^
    --  |  |
    --  |  +----- the type for expressions
    --  +-------- the type for identifiers
    = Empty
    | Bind id Ref (Env id e) -- name reference
    | RecBind (List (RecBinding id e)) (Env id e) -- [( name, param, body )]


type alias RecBinding id e =
    { name : id
    , param : id
    , body : e
    }


empty : Env id e
empty =
    Empty


extend : id -> Ref -> Env id e -> Env id e
extend =
    Bind


extendRec : List (RecBinding id e) -> Env id e -> Env id e
extendRec =
    RecBind


type Item v
    = Ref Ref
    | Procedure Ref (Store v)


find :
    { needle : id
    , toClosure : id -> e -> Env id e -> v
    , store0 : Store v
    }
    -> Env id e
    -> Maybe (Item v)
find options env =
    case env of
        Empty ->
            Nothing

        Bind name ref restEnv ->
            if options.needle == name then
                Just <| Ref ref

            else
                find options restEnv

        RecBind recBindings restEnv ->
            case findRecBinding options.needle recBindings of
                Just { param, body } ->
                    let
                        ( ref, store1 ) =
                            Store.newref (options.toClosure param body env) options.store0
                    in
                    Just <| Procedure ref store1

                Nothing ->
                    find options restEnv


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
