module Ch5.MUTEX.Examples exposing (example1, example2, example3)

import Ch5.MUTEX.Interpreter as I exposing (Error, State(..), Value)
import Ch5.THREADS.Output as Output exposing (Output)


example1 : Int -> ( Result Error Value, List String )
example1 maxTimeSlice =
    --
    -- Example 1: Two threads showing interleaved computation.
    --
    -- NOTE: You can reproduce the output shown in the book
    --       by using maxTimeSlice = 10.
    --
    """
    letrec
        noisy(l) =
            if null?(l) then
                0
            else
                begin
                    print(car(l));
                    (noisy cdr(l))
                end
    in
    begin
        spawn(proc (d) (noisy list(1, 2, 3, 4, 5)));
        spawn(proc (d) (noisy list(6, 7, 8, 9, 10)));
        print(100);
        33
    end
    """
        |> run maxTimeSlice


example2 : Int -> ( Result Error Value, List String )
example2 maxTimeSlice =
    --
    -- Example 2: A producer and consumer, linked by a buffer.
    --
    """
    let
        buffer = 0
    in
    let
        producer =
            proc (n)
                letrec
                    wait(k) =
                        if zero?(k) then
                            set buffer = n
                        else
                            begin
                                print(-(k, -(0, 200)));
                                (wait -(k, 1))
                            end
                in
                (wait 5)
    in
    let
        consumer =
            proc (d)
                letrec
                    busywait(k) =
                        if zero?(buffer) then
                            begin
                                print(-(k, -(0, 100)));
                                (busywait -(k, -(0, 1)))
                            end
                        else
                            buffer
                in
                (busywait 0)
    in
    begin
        spawn(proc (d) (producer 44));
        print(300);
        (consumer 86)
    end
    """
        |> run maxTimeSlice



--
-- Results for example2 from a test run with maxTimeSlice = 43:
--
-- ( Ok (VNumber 44)
-- , [ "300"
--   , "100"
--   , "101"
--   , "205"
--   , "204"
--   , "203"
--   , "102"
--   , "103"
--   , "104"
--   , "202"
--   , "201"
--   , "End of sub-thread computation"
--   , "105"
--   , "End of main thread computation"
--   ]
-- )
--


run : Int -> String -> ( Result Error Value, List String )
run maxTimeSlice input =
    let
        ( result, State { output } ) =
            I.run maxTimeSlice input
    in
    ( result, Output.toList output )


example3 : Int -> ( Result Error Value, State )
example3 maxTimeSlice =
    --
    -- Example 3: An unsafe counter.
    --
    """
    let
        x = 0
    in
    let
        incrx =
            proc (id)
                proc (dummy)
                    set x = -(x, -(0, 1))
    in
    begin
        spawn((incrx 100));
        spawn((incrx 200));
        spawn((incrx 300))
    end
    """
        |> I.run maxTimeSlice



--
-- Results for example3:
--
-- I tried example3 with maxTimeSlice = 1 to 40 and I wasn't able
-- to make the counter become 1 or 2. It was always set to 3.
--
-- However, if I use `set x = -(x, -(0, -(2, 1)))` then with
-- maxTimeSlice = 1 the final value of x is 2 and not 3. For a
-- large enough value of maxTimeSlice, 4 in this case, the final
-- value of x is 3.
--
