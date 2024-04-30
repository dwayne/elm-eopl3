module Test.Ch4.EXPLICIT_REFS.Store exposing (suite)

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref, Store)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        store0 =
            Store.empty

        ( r1, store1 ) =
            Store.newref 5 store0

        ( r2, store2 ) =
            Store.newref 10 store1
    in
    describe "Ch4.EXPLICIT_REFS.Store"
        [ describe "deref"
            [ test "deref r1 == 5" <|
                \_ ->
                    deref r1 store2
                        |> Expect.equal 5
            , test "deref r2 == 10" <|
                \_ ->
                    deref r2 store2
                        |> Expect.equal 10
            ]
        , let
            store3 =
                Store.setref r1 (deref r2 store2) store2

            store4 =
                Store.setref r2 15 store3
          in
          describe "setref"
            [ test "setref r1 (deref r2); deref r1 == 10" <|
                \_ ->
                    deref r1 store4
                        |> Expect.equal 10
            , test "setref r2 15; deref r2 == 15" <|
                \_ ->
                    deref r2 store4
                        |> Expect.equal 15
            ]
        ]



-- HELPERS


deref : Ref -> Store Int -> Int
deref ref =
    Store.deref ref >> Maybe.withDefault 0
