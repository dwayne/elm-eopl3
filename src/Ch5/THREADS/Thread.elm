module Ch5.THREADS.Thread exposing (Thread, new, run)


type Thread a
    = Thread (() -> a)


new : (() -> a) -> Thread a
new =
    Thread


run : Thread a -> a
run (Thread f) =
    f ()
