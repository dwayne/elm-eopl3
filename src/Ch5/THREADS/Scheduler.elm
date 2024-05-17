module Ch5.THREADS.Scheduler exposing
    ( Scheduler
    , isTimeExpired
    , new
    , runNextThread
    , schedule
    , setFinalAnswer
    , tick
    )

import Ch5.THREADS.Queue as Queue exposing (Queue)
import Ch5.THREADS.Thread as Thread exposing (Thread)


type Scheduler a
    = Scheduler
        { readyQueue : Queue (Thread a)
        , maybeFinalAnswer : Maybe a
        , maxTimeSlice : Int
        , timeRemaining : Int
        }


new : Int -> Scheduler a
new n =
    let
        maxTimeSlice =
            max 1 n
    in
    Scheduler
        { readyQueue = Queue.empty
        , maybeFinalAnswer = Nothing
        , maxTimeSlice = maxTimeSlice
        , timeRemaining = maxTimeSlice
        }


schedule : Thread a -> Scheduler a -> Scheduler a
schedule thread (Scheduler state) =
    Scheduler
        { state
            | readyQueue = Queue.enqueue thread state.readyQueue
        }


setFinalAnswer : a -> Scheduler a -> Scheduler a
setFinalAnswer finalAnswer (Scheduler state) =
    Scheduler
        { state
            | maybeFinalAnswer = Just finalAnswer
        }


isTimeExpired : Scheduler a -> Bool
isTimeExpired (Scheduler { timeRemaining }) =
    timeRemaining == 0


tick : Scheduler a -> Scheduler a
tick (Scheduler state) =
    Scheduler
        { state
            | timeRemaining = max 0 (state.timeRemaining - 1)
        }


runNextThread : Scheduler a -> Maybe ( a, Scheduler a )
runNextThread ((Scheduler state) as scheduler) =
    case Queue.dequeue state.readyQueue of
        Just ( thread, readyQueue ) ->
            Just
                ( Thread.run thread
                , Scheduler
                    { state
                        | readyQueue = readyQueue
                        , timeRemaining = state.maxTimeSlice
                    }
                )

        Nothing ->
            state.maybeFinalAnswer
                |> Maybe.map
                    (\finalAnswer ->
                        ( finalAnswer
                        , scheduler
                        )
                    )
