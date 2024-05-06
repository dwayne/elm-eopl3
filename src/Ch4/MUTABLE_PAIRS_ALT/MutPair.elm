module Ch4.MUTABLE_PAIRS_ALT.MutPair exposing
    ( MutPair
    , left
    , new
    , right
    , setLeft
    , setRight
    , toString
    )

import Ch4.MUTABLE_PAIRS_ALT.Store as Store exposing (Ref, Store)


type
    MutPair
    --
    -- Represent a pair by a reference to its left.
    --
    = MutPair Ref


new : a -> a -> Store a -> ( MutPair, Store a )
new leftValue rightValue store0 =
    let
        ( leftRef, store1 ) =
            Store.newref leftValue store0

        ( _, store2 ) =
            Store.newref rightValue store1
    in
    ( MutPair leftRef
    , store2
    )


left : Store a -> MutPair -> Maybe a
left store (MutPair leftRef) =
    Store.deref leftRef store


right : Store a -> MutPair -> Maybe a
right store (MutPair leftRef) =
    Store.deref (Store.refNext leftRef) store


setLeft : a -> Store a -> MutPair -> Store a
setLeft leftValue store (MutPair leftRef) =
    Store.setref leftRef leftValue store


setRight : a -> Store a -> MutPair -> Store a
setRight rightValue store (MutPair leftRef) =
    Store.setref (Store.refNext leftRef) rightValue store


toString : MutPair -> String
toString (MutPair leftRef) =
    "MutPair (" ++ Store.refToString leftRef ++ ") (" ++ Store.refToString (Store.refNext leftRef) ++ ")"
