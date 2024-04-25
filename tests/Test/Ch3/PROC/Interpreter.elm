module Test.Ch3.PROC.Interpreter exposing (suite)

import Ch3.PROC.Interpreter as I exposing (Value(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch3.PROC.Interpreter" <|
        List.map testValue
            [ ( "5"
              , VNumber 5
              )
            , ( "x"
              , VNumber 10
              )
            , ( "zero?(i)"
              , VBool False
              )
            , ( "zero?(-(i, 1))"
              , VBool True
              )
            , ( "-(55, -(x, 11))"
              , VNumber 56
              )
            , ( "-(-(x, 3), -(v, i))"
              , VNumber 3
              )
            , ( """
                let x = 33
                in let y = 22
                   in if zero?(-(x, 11)) then -(y, 2) else -(y, 4)
                """
              , VNumber 18
              )
            , ( "let x = 5 in -(x, 3)"
              , VNumber 2
              )
            , ( """
                let z = 5 in
                  let x = 3 in
                    let y = -(x, 1) in
                      let x = 4 in -(z, -(x, y))
                """
              , VNumber 3
              )
            , ( """
                let x = 7 in
                  let y = 2 in
                    let y = let x = -(x, 1) in -(x, y) in
                      -(-(x, 8), y)
                """
              , VNumber -5
              )
            ]


testValue : ( String, Value ) -> Test
testValue ( input, expectedValue ) =
    test input <|
        \_ ->
            case I.run input of
                Ok actualValue ->
                    if expectedValue == actualValue then
                        Expect.pass

                    else
                        Expect.fail <| "expected = " ++ Debug.toString expectedValue ++ ", actual = " ++ Debug.toString actualValue

                Err e ->
                    Expect.fail <| Debug.toString e
