module Test.Ch5.MUTEX.Interpreter exposing (suite)

import Ch5.MUTEX.Interpreter as I exposing (State(..), Value(..))
import Ch5.THREADS.Output as Output
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch5.MUTEX.Interpreter" <|
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
            , ( """
                let f = proc (x) -(x, 11)
                in (f (f 77))
                """
              , VNumber 55
              )
            , ( """
                (proc (f) (f (f 77))
                 proc (x) -(x, 11))
                """
              , VNumber 55
              )
            , ( """
                let x = 200
                in let f = proc (z) -(z, x)
                   in let x = 100
                      in let g = proc (z) -(z, x)
                         in -((f 1), (g 1))
                """
              , VNumber -100
              )
            , ( """
                let sum = proc (x) proc (y) -(x, -(0, y))
                in ((sum 3) 4)
                """
                --
                -- Example for exercise 3.20
                --
              , VNumber 7
              )
            , ( """
                let makemult =
                  proc (maker)
                    proc (x)
                      if zero?(x) then
                        0
                      else
                        -(((maker maker) -(x, 1)), -(0, 4))
                in let timesfour = proc (x) ((makemult makemult) x)
                   in (timesfour 3)
                """
                --
                -- Example for exercise 3.23 - timesfour
                --
                -- Changes:
                -- 1. -4 = -(0, 4)
                -- 2. times4 = timesfour
                --
              , VNumber 12
              )
            , ( """
                let timesmaker =
                  proc (maker)
                    proc (x)
                      proc (y)
                        if zero?(y) then
                          0
                        else
                          -((((maker maker) x) -(y, 1)), -(0, x))
                in let times = proc (x) proc (y) (((timesmaker timesmaker) x) y)
                   in let factmaker =
                        proc (maker)
                          proc (n)
                            if zero?(n) then
                              1
                            else
                              ((times n) ((maker maker) -(n, 1)))
                      in let fact = proc (n) ((factmaker factmaker) n)
                         in (fact 5)
                """
                --
                -- Example for exercise 3.23 - fact
                --
              , VNumber 120
              )
            , ( """
                let timesmaker =
                  proc (maker)
                    proc (x)
                      proc (y)
                        if zero?(y) then
                          0
                        else
                          -((((maker maker) x) -(y, 1)), -(0, x))
                in let times = (timesmaker timesmaker)
                   in let factmaker =
                        proc (maker)
                          proc (n)
                            if zero?(n) then
                              1
                            else
                              ((times n) ((maker maker) -(n, 1)))
                      in let fact = (factmaker factmaker)
                         in (fact 5)
                """
                --
                -- Example for exercise 3.23 - fact (alternative)
                --
                -- N.B.
                -- times = (timesmaker timesmaker)
                -- fact = (factmaker factmaker)
                --
              , VNumber 120
              )
            , ( """
                let evenmaker =
                  proc (evenmaker)
                    proc (oddmaker)
                      proc (x)
                        if zero?(x) then
                          1
                        else
                          (((oddmaker oddmaker) evenmaker) -(x, 1))
                in let oddmaker =
                     proc (oddmaker)
                       proc (evenmaker)
                         proc (x)
                           if zero?(x) then
                             0
                           else
                             (((evenmaker evenmaker) oddmaker) -(x, 1))
                   in let odd = ((oddmaker oddmaker) evenmaker)
                      in (odd 13)
                """
                --
                -- Example for exercise 3.24 - odd and even
                --
              , VNumber 1
              )
            , ( """
                let makerec =
                  proc (f)
                    let d =
                      proc (x)
                        proc (z) ((f (x x)) z)
                    in proc (n) ((f (d d)) n)
                in let maketimes =
                     proc (f)
                       proc (x)
                         if zero?(x) then
                           0
                         else
                           -((f -(x, 1)), -(0, 4))
                   in let times = (makerec maketimes)
                      in (times 3)
                """
                --
                -- Example for exercise 3.25 - from the book
                --
                -- Changes:
                -- 1. maketimes4 = maketimes
                -- 2. times4 = times
                -- 3. -4 = -(0, 4)
                --
              , VNumber 12
              )
            , ( """
                letrec double(x)
                  = if zero?(x) then 0 else -((double -(x,1)), -(0,2))
                in (double 6)
                """
              , VNumber 12
              )
            , ( """
                let f = proc (x) proc (y)
                          begin
                            set x = -(x, -(0, 1));
                            -(x,y)
                          end
                in ((f 44) 33)
                """
              , VNumber 12
              )
            , ( """
                let x = 0
                in letrec even(dummy)
                          = if zero?(x)
                            then 1
                            else begin
                                   set x = -(x, 1);
                                   (odd 888)
                                 end
                          odd(dummy)
                          = if zero?(x)
                            then 0
                            else begin
                                   set x = -(x, 1);
                                   (even 888)
                                 end
                   in begin set x = 13; (odd 888) end
                """
              , VNumber 1
              )
            , ( """
                let g = let counter = 0
                        in proc(dummy)
                             begin
                               set counter = -(counter, -(0, 1));
                               counter
                             end
                in let a = (g 11)
                   in let b = (g 11)
                      in -(a, b)
                """
              , VNumber -1
              )
            , ( """
                emptylist
                """
              , VList []
              )
            , ( """
                cons(1, emptylist)
                """
              , VList [ VNumber 1 ]
              )
            , ( """
                cons(1, cons(2, cons(3, cons(zero?(0), emptylist))))
                """
              , VList [ VNumber 1, VNumber 2, VNumber 3, VBool True ]
              )
            , ( """
                car(cons(1, emptylist))
                """
              , VNumber 1
              )
            , ( """
                cdr(cons(1, cons(2, emptylist)))
                """
              , VList [ VNumber 2 ]
              )
            , ( """
                null?(emptylist)
                """
              , VBool True
              )
            , ( """
                null?(cons(1, emptylist))
                """
              , VBool False
              )
            , ( """
                list()
                """
              , VList []
              )
            , ( """
                list(5)
                """
              , VList [ VNumber 5 ]
              )
            , ( """
                list(5, zero?(10), 15)
                """
              , VList [ VNumber 5, VBool False, VNumber 15 ]
              )
            ]
            ++ List.map testOutput
                [ ( """
                    print(x)
                    """
                  , [ "10", "End of main thread computation" ]
                  )
                , ( """
                    begin
                        print(x);
                        print(v);
                        print(i)
                    end
                    """
                  , [ "10", "5", "1", "End of main thread computation" ]
                  )
                ]


testValue : ( String, Value ) -> Test
testValue ( input, expectedValue ) =
    test input <|
        \_ ->
            case I.run 10 input of
                ( Ok actualValue, _ ) ->
                    if expectedValue == actualValue then
                        Expect.pass

                    else
                        Expect.fail <| "expected = " ++ Debug.toString expectedValue ++ ", actual = " ++ Debug.toString actualValue

                ( Err e, _ ) ->
                    Expect.fail <| Debug.toString e


testOutput : ( String, List String ) -> Test
testOutput ( input, expectedOutput ) =
    test input <|
        \_ ->
            case I.run 10 input of
                ( Ok _, State { output } ) ->
                    let
                        actualOutput =
                            Output.toList output
                    in
                    if expectedOutput == actualOutput then
                        Expect.pass

                    else
                        Expect.fail <| "expected = " ++ Debug.toString expectedOutput ++ ", actual = " ++ Debug.toString actualOutput

                ( Err e, _ ) ->
                    Expect.fail <| Debug.toString e