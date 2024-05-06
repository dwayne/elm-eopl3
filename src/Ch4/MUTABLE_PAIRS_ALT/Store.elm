module Ch4.MUTABLE_PAIRS_ALT.Store exposing
    ( Ref
    , Store
    , deref
    , empty
    , newref
    , refNext
    , refToString
    , setref
    )


type Store a
    = Store (List a)


type Ref
    = Ref Int


empty : Store a
empty =
    Store []


newref : a -> Store a -> ( Ref, Store a )
newref value (Store locations) =
    let
        r =
            List.length locations
    in
    ( Ref r
    , Store <| List.append locations [ value ]
    )


deref : Ref -> Store a -> Maybe a
deref (Ref r) (Store locations) =
    locations
        |> List.drop r
        |> List.head


setref : Ref -> a -> Store a -> Store a
setref (Ref r) newValue (Store locations) =
    Store <|
        List.indexedMap
            (\i previousValue ->
                if r == i then
                    newValue

                else
                    previousValue
            )
            locations


refToString : Ref -> String
refToString (Ref r) =
    "Ref " ++ String.fromInt r


refNext : Ref -> Ref
refNext (Ref r) =
    Ref (r + 1)
