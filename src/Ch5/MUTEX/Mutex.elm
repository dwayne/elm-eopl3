module Ch5.MUTEX.Mutex exposing
    ( Mutex
    , Status(..)
    , close
    , dequeue
    , enqueue
    , getStatus
    , new
    , open
    )

import Ch5.THREADS.Thread exposing (Thread)
import Lib.Queue as Queue exposing (Queue)


type Mutex a
    = Mutex
        { status : Status
        , waitQueue : Queue (Thread a)
        }


type Status
    = Open
    | Closed


new : Mutex a
new =
    Mutex
        { status = Open
        , waitQueue = Queue.empty
        }


getStatus : Mutex a -> Status
getStatus (Mutex { status }) =
    status


open : Mutex a -> Mutex a
open (Mutex state) =
    Mutex { state | status = Open }


close : Mutex a -> Mutex a
close (Mutex state) =
    Mutex { state | status = Closed }


enqueue : Thread a -> Mutex a -> Mutex a
enqueue thread (Mutex state) =
    Mutex { state | waitQueue = Queue.enqueue thread state.waitQueue }


dequeue : Mutex a -> Maybe ( Thread a, Mutex a )
dequeue (Mutex state) =
    state.waitQueue
        |> Queue.dequeue
        |> Maybe.map
            (\( thread, newWaitQueue ) ->
                ( thread
                , Mutex { state | waitQueue = newWaitQueue }
                )
            )
