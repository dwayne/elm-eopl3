module Ch5.THREADS.Queue exposing
    ( Queue
    , dequeue
    , empty
    , enqueue
    )


type Queue a
    = Queue (List a) (List a)


empty : Queue a
empty =
    Queue [] []


enqueue : a -> Queue a -> Queue a
enqueue x (Queue front back) =
    Queue front (x :: back)


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue ((Queue front back) as q) =
    case ( front, back ) of
        ( [], [] ) ->
            ( Nothing, q )

        ( x :: restFront, _ ) ->
            ( Just x, Queue restFront back )

        _ ->
            dequeue (Queue (List.reverse back) [])
