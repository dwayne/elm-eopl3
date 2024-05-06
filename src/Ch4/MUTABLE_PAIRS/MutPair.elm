module Ch4.MUTABLE_PAIRS.MutPair exposing
    ( MutPair
    , new
    , left
    , right
    , setLeft
    , setRight
    )


import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref, Store)


type MutPair
    = MutPair Ref Ref


new : a -> a -> Store a -> ( MutPair, Store a )
new leftValue rightValue store0 =
    let
        ( leftRef, store1 ) =
            Store.newref leftValue store0

        ( rightRef, store2 ) =
            Store.newref rightValue store1
    in
    ( MutPair leftRef rightRef
    , store2
    )


left : Store a -> MutPair -> Maybe a
left store (MutPair leftRef _) =
    Store.deref leftRef store


right : Store a -> MutPair -> Maybe a
right store (MutPair _ rightRef) =
    Store.deref rightRef store


setLeft : a -> Store a -> MutPair -> Store a
setLeft leftValue store (MutPair leftRef _) =
    Store.setref leftRef leftValue store


setRight : a -> Store a -> MutPair -> Store a
setRight rightValue store (MutPair _ rightRef) =
    Store.setref rightRef rightValue store
