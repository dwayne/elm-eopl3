module Test.Helpers exposing
    ( testMaybe
    , testOk
    , testResult
    , testResultBy
    , testResultWith
    )

import Expect
import Test exposing (Test, test)


testMaybe : (input -> Maybe a) -> ( input, Maybe a ) -> Test
testMaybe f =
    testOk (f >> Result.fromMaybe ())


testOk : (input -> Result x a) -> ( input, Maybe a ) -> Test
testOk f ( input, expected ) =
    testResultBy f (always ()) ( input, Result.fromMaybe () expected )


testResult : (input -> Result x a) -> ( input, Result x a ) -> Test
testResult f =
    testResultWith f (==)


testResultBy : (input -> Result x a) -> (x -> y) -> ( input, Result y a ) -> Test
testResultBy f toY =
    testResultWith f (\x y -> toY x == y)


testResultWith : (input -> Result x a) -> (x -> y -> Bool) -> ( input, Result y a ) -> Test
testResultWith f isEqual ( input, expectedOutput ) =
    test (Debug.toString input) <|
        \_ ->
            case ( f input, expectedOutput ) of
                ( (Ok actual) as actualOutput, Ok expected ) ->
                    if actual == expected then
                        Expect.pass

                    else
                        Expect.fail <|
                            Debug.toString
                                { actual = actualOutput
                                , expected = expectedOutput
                                }

                ( (Err x) as actualOutput, Err y ) ->
                    if isEqual x y then
                        Expect.pass

                    else
                        Expect.fail <|
                            Debug.toString
                                { actual = actualOutput
                                , expected = expectedOutput
                                }

                ( actualOutput, _ ) ->
                    Expect.fail <|
                        Debug.toString
                            { actual = actualOutput
                            , expected = expectedOutput
                            }
