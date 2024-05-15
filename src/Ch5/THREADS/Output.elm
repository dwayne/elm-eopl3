module Ch5.THREADS.Output exposing
    ( Output
    , empty
    , print
    , toList
    )


type Output
    = Output (List String)


empty : Output
empty =
    Output []


print : String -> Output -> Output
print s (Output o) =
    Output <| s :: o


toList : Output -> List String
toList (Output o) =
    List.reverse o
