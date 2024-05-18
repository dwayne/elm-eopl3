module Ch5.MUTEX.Interpreter exposing
    ( Error(..)
    , State(..)
    , Value(..)
    , run
    )

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref)
import Ch4.IMPLICIT_REFS.Env as Env
import Ch5.MUTEX.AST as AST exposing (..)
import Ch5.MUTEX.Mutex as Mutex
import Ch5.MUTEX.Parser as P
import Ch5.THREADS.Output as Output exposing (Output)
import Ch5.THREADS.Scheduler as Scheduler exposing (Scheduler)
import Ch5.THREADS.Thread as Thread exposing (Thread)



--
-- TODO:
--
-- [x] Start with THREADS
-- [x] Implement a mutex data structure
-- [x] Add syntax for mutex, wait, and signal
-- [x] Implement Mutex
-- [x] Implement Wait
-- [x] Implement Signal
--


type Value
    = VUnit
    | VNumber Number
    | VBool Bool
    | VList (List Value)
    | VProcedure Procedure
    | VMutex Mutex
    | VRef Ref


type alias Env =
    Env.Env Id Expr


type alias Store =
    Store.Store Value


type State
    = State
        { store : Store
        , scheduler : Scheduler (State -> ( Result RuntimeError Value, State ))
        , output : Output
        }


type Procedure
    = Closure Id Expr Env


type alias Mutex =
    Mutex.Mutex (State -> ( Result RuntimeError Value, State ))


type Type
    = TUnit
    | TNumber
    | TBool
    | TList
    | TProcedure
    | TMutex
    | TRef


type Error
    = SyntaxError P.Error
    | RuntimeError RuntimeError


type RuntimeError
    = IdentifierNotFound Id
    | ReferenceNotFound Ref
    | EmptyListError
    | TypeError
        { expected : List Type
        , actual : List Type
        }
    | UnexpectedError String


run : Int -> String -> ( Result Error Value, State )
run maxTimeSlice input =
    let
        ( initEnv, initState ) =
            initEnvAndState maxTimeSlice
    in
    case P.parse input of
        Ok program ->
            evalProgram program initEnv initState
                |> Tuple.mapFirst (Result.mapError RuntimeError)

        Err err ->
            ( Err <| SyntaxError err
            , initState
            )


initEnvAndState : Int -> ( Env, State )
initEnvAndState maxTimeSlice =
    let
        store0 =
            Store.empty

        ( xRef, store1 ) =
            Store.newref (VNumber 10) store0

        ( vRef, store2 ) =
            Store.newref (VNumber 5) store1

        ( iRef, store3 ) =
            Store.newref (VNumber 1) store2
    in
    ( Env.empty
        |> Env.extend "x" xRef
        |> Env.extend "v" vRef
        |> Env.extend "i" iRef
    , State
        { store = store3
        , scheduler = Scheduler.new maxTimeSlice
        , output = Output.empty
        }
    )


type Continuation
    = EndMainThreadCont
    | EndSubThreadCont
    | ZeroCont Continuation
    | LetCont Id Expr Env Continuation
    | IfCont Expr Expr Env Continuation
    | Diff1Cont Expr Env Continuation
    | Diff2Cont Value Continuation
    | Cons1Cont Expr Env Continuation
    | Cons2Cont Value Continuation
    | CarCont Continuation
    | CdrCont Continuation
    | NullCont Continuation
    | ListCont (List Value) (List Expr) Env Continuation
    | RatorCont Expr Env Continuation
    | RandCont Procedure Continuation
    | SetCont Id Env Continuation
    | SequenceCont (List Expr) Env Continuation
    | PrintCont Continuation
    | SpawnCont Continuation
    | WaitCont Continuation
    | SignalCont Continuation


evalProgram : AST.Program -> Env -> State -> ( Result RuntimeError Value, State )
evalProgram (Program expr) =
    evalExpr expr EndMainThreadCont


evalExpr : Expr -> Continuation -> Env -> State -> ( Result RuntimeError Value, State )
evalExpr expr cont env state =
    case expr of
        Const n ->
            applyCont cont (Ok <| VNumber n) state

        Var name ->
            let
                ( result1, state1 ) =
                    find name env state

                result2 =
                    case result1 of
                        Ok ref ->
                            deref ref state1

                        Err err ->
                            Err err
            in
            applyCont cont result2 state1

        Diff a b ->
            evalExpr a (Diff1Cont b env cont) env state

        Zero a ->
            evalExpr a (ZeroCont cont) env state

        Cons a b ->
            evalExpr a (Cons1Cont b env cont) env state

        Car a ->
            evalExpr a (CarCont cont) env state

        Cdr a ->
            evalExpr a (CdrCont cont) env state

        Null a ->
            evalExpr a (NullCont cont) env state

        EmptyList ->
            applyCont cont (Ok <| VList []) state

        List exprs ->
            case exprs of
                [] ->
                    applyCont cont (Ok <| VList []) state

                firstExpr :: restExprs ->
                    evalExpr firstExpr (ListCont [] restExprs env cont) env state

        If test consequent alternative ->
            evalExpr test (IfCont consequent alternative env cont) env state

        Let name e body ->
            evalExpr e (LetCont name body env cont) env state

        Proc param body ->
            applyCont cont (Ok <| VProcedure <| Closure param body env) state

        Letrec procrecs letrecBody ->
            evalExpr letrecBody cont (Env.extendRec procrecs env) state

        Call rator rand ->
            evalExpr rator (RatorCont rand env cont) env state

        Set name e ->
            evalExpr e (SetCont name env cont) env state

        Begin firstExpr restExprs ->
            evalExpr firstExpr (SequenceCont restExprs env cont) env state

        Print e ->
            evalExpr e (PrintCont cont) env state

        Spawn e ->
            evalExpr e (SpawnCont cont) env state

        Mutex ->
            newMutex cont state

        Wait e ->
            evalExpr e (WaitCont cont) env state

        Signal e ->
            evalExpr e (SignalCont cont) env state


applyCont : Continuation -> Result RuntimeError Value -> State -> ( Result RuntimeError Value, State )
applyCont cont result startState =
    if isTimeExpired startState then
        let
            state =
                schedule thread startState

            thread =
                Thread.new (\_ -> applyCont cont result)
        in
        runNextThread state

    else
        let
            state =
                tick startState
        in
        case cont of
            EndMainThreadCont ->
                let
                    state1 =
                        print "End of main thread computation" state

                    state2 =
                        setFinalAnswer (\s -> ( result, s )) state1
                in
                runNextThread state2

            EndSubThreadCont ->
                let
                    state1 =
                        print "End of sub-thread computation" state
                in
                runNextThread state1

            ZeroCont nextCont ->
                case result of
                    Ok va ->
                        applyCont nextCont (evalZero va) state

                    Err _ ->
                        applyCont nextCont result state

            LetCont name body env nextCont ->
                case result of
                    Ok ve ->
                        let
                            ( resultRef, state1 ) =
                                newref ve state
                        in
                        case resultRef of
                            Ok ref ->
                                evalExpr body nextCont (Env.extend name ref env) state1

                            Err err ->
                                applyCont nextCont (Err err) state1

                    Err _ ->
                        applyCont nextCont result state

            IfCont consequent alternative env nextCont ->
                case result of
                    Ok vTest ->
                        evalIf vTest consequent alternative nextCont env state

                    Err _ ->
                        applyCont nextCont result state

            Diff1Cont b env nextCont ->
                case result of
                    Ok va ->
                        evalExpr b (Diff2Cont va nextCont) env state

                    Err _ ->
                        applyCont nextCont result state

            Diff2Cont va nextCont ->
                case result of
                    Ok vb ->
                        applyCont nextCont (evalDiff va vb) state

                    Err _ ->
                        applyCont nextCont result state

            Cons1Cont b env nextCont ->
                case result of
                    Ok va ->
                        evalExpr b (Cons2Cont va nextCont) env state

                    Err _ ->
                        applyCont nextCont result state

            Cons2Cont va nextCont ->
                case result of
                    Ok vb ->
                        applyCont nextCont (evalCons va vb) state

                    Err _ ->
                        applyCont nextCont result state

            CarCont nextCont ->
                case result of
                    Ok va ->
                        applyCont nextCont (evalCar va) state

                    Err _ ->
                        applyCont nextCont result state

            CdrCont nextCont ->
                case result of
                    Ok va ->
                        applyCont nextCont (evalCdr va) state

                    Err _ ->
                        applyCont nextCont result state

            NullCont nextCont ->
                case result of
                    Ok va ->
                        applyCont nextCont (evalNull va) state

                    Err _ ->
                        applyCont nextCont result state

            ListCont prevRevValues exprs env nextCont ->
                case result of
                    Ok value ->
                        let
                            revValues =
                                value :: prevRevValues
                        in
                        case exprs of
                            [] ->
                                applyCont nextCont (Ok <| VList <| List.reverse revValues) state

                            firstExpr :: restExprs ->
                                evalExpr firstExpr (ListCont revValues restExprs env nextCont) env state

                    Err _ ->
                        applyCont nextCont result state

            RatorCont rand env nextCont ->
                case result of
                    Ok vRator ->
                        let
                            resultF =
                                toProcedure vRator
                        in
                        case resultF of
                            Ok f ->
                                evalExpr rand (RandCont f nextCont) env state

                            Err err ->
                                applyCont nextCont (Err err) state

                    Err _ ->
                        applyCont nextCont result state

            RandCont f nextCont ->
                case result of
                    Ok vRand ->
                        applyProcedure f vRand nextCont state

                    Err _ ->
                        applyCont nextCont result state

            SetCont name env nextCont ->
                case result of
                    Ok ve ->
                        let
                            ( findResult, state1 ) =
                                find name env state
                        in
                        case findResult of
                            Ok ref ->
                                setref ve ref state1
                                    |> applyCont nextCont (Ok VUnit)

                            Err err ->
                                applyCont nextCont (Err err) state1

                    Err _ ->
                        applyCont nextCont result state

            SequenceCont exprs env nextCont ->
                case exprs of
                    [] ->
                        applyCont nextCont result state

                    expr :: restExprs ->
                        evalExpr expr (SequenceCont restExprs env nextCont) env state

            PrintCont nextCont ->
                case result of
                    Ok value ->
                        applyCont nextCont (Ok VUnit) (printValue value state)

                    Err _ ->
                        applyCont nextCont result state

            SpawnCont nextCont ->
                case result of
                    Ok value ->
                        case toProcedure value of
                            Ok f ->
                                let
                                    state1 =
                                        schedule thread state

                                    thread =
                                        Thread.new
                                            (\_ ->
                                                applyProcedure f VUnit EndSubThreadCont
                                            )
                                in
                                applyCont nextCont (Ok VUnit) state1

                            Err err ->
                                applyCont nextCont (Err err) state

                    Err _ ->
                        applyCont nextCont result state

            WaitCont nextCont ->
                case result of
                    Ok value ->
                        case toMutex value state of
                            Ok ( ref, m ) ->
                                wait ref m nextCont state

                            Err err ->
                                applyCont nextCont (Err err) state

                    Err _ ->
                        applyCont nextCont result state

            SignalCont nextCont ->
                case result of
                    Ok value ->
                        case toMutex value state of
                            Ok ( ref, m ) ->
                                signal ref m nextCont state

                            Err err ->
                                applyCont nextCont (Err err) state

                    Err _ ->
                        applyCont nextCont result state


evalDiff : Value -> Value -> Result RuntimeError Value
evalDiff va vb =
    case ( va, vb ) of
        ( VNumber a, VNumber b ) ->
            Ok <| VNumber <| a - b

        _ ->
            Err <|
                TypeError
                    { expected = [ TNumber, TNumber ]
                    , actual = [ typeOf va, typeOf vb ]
                    }


evalZero : Value -> Result RuntimeError Value
evalZero va =
    case va of
        VNumber n ->
            Ok <| VBool <| n == 0

        _ ->
            Err <|
                TypeError
                    { expected = [ TNumber ]
                    , actual = [ typeOf va ]
                    }


evalIf : Value -> Expr -> Expr -> Continuation -> Env -> State -> ( Result RuntimeError Value, State )
evalIf vTest consequent alternative cont env state =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent cont env state

            else
                evalExpr alternative cont env state

        _ ->
            let
                result =
                    Err <|
                        TypeError
                            { expected = [ TBool ]
                            , actual = [ typeOf vTest ]
                            }
            in
            applyCont cont result state


evalCons : Value -> Value -> Result RuntimeError Value
evalCons va vb =
    toList vb
        |> Result.map (VList << (::) va)


evalCar : Value -> Result RuntimeError Value
evalCar va =
    toList va
        |> Result.andThen
            (\list ->
                case list of
                    h :: _ ->
                        Ok h

                    [] ->
                        Err EmptyListError
            )


evalCdr : Value -> Result RuntimeError Value
evalCdr va =
    toList va
        |> Result.andThen
            (\list ->
                case list of
                    _ :: t ->
                        Ok <| VList t

                    [] ->
                        Err EmptyListError
            )


evalNull : Value -> Result RuntimeError Value
evalNull va =
    toList va
        |> Result.map (VBool << List.isEmpty)


applyProcedure : Procedure -> Value -> Continuation -> State -> ( Result RuntimeError Value, State )
applyProcedure (Closure param body savedEnv) value cont state =
    let
        ( result, state1 ) =
            newref value state
    in
    case result of
        Ok ref ->
            evalExpr body cont (Env.extend param ref savedEnv) state1

        Err err ->
            applyCont cont (Err err) state1


toProcedure : Value -> Result RuntimeError Procedure
toProcedure v =
    case v of
        VProcedure f ->
            Ok f

        _ ->
            Err <|
                TypeError
                    { expected = [ TProcedure ]
                    , actual = [ typeOf v ]
                    }


toList : Value -> Result RuntimeError (List Value)
toList v =
    case v of
        VList list ->
            Ok list

        _ ->
            Err <|
                TypeError
                    { expected = [ TList ]
                    , actual = [ typeOf v ]
                    }


toMutex : Value -> State -> Result RuntimeError ( Ref, Mutex )
toMutex v state =
    case v of
        VRef ref ->
            case deref ref state of
                Ok (VMutex m) ->
                    Ok ( ref, m )

                Ok actualValue ->
                    Err <|
                        TypeError
                            { expected = [ TMutex ]
                            , actual = [ typeOf actualValue ]
                            }

                Err err ->
                    Err err

        _ ->
            Err <|
                TypeError
                    { expected = [ TRef ]
                    , actual = [ typeOf v ]
                    }


typeOf : Value -> Type
typeOf v =
    case v of
        VUnit ->
            TUnit

        VNumber _ ->
            TNumber

        VBool _ ->
            TBool

        VList _ ->
            TList

        VProcedure _ ->
            TProcedure

        VMutex _ ->
            TMutex

        VRef _ ->
            TRef


toString : Value -> String
toString v =
    case v of
        VUnit ->
            "()"

        VNumber n ->
            String.fromInt n

        VBool b ->
            if b then
                "true"

            else
                "false"

        VList list ->
            String.concat
                [ "["
                , list
                    |> List.map toString
                    |> String.join ", "
                , "]"
                ]

        VProcedure _ ->
            "<<proc>>"

        VMutex _ ->
            "<<mutex>>"

        VRef ref ->
            Store.refToString ref



-- MUTEX


newMutex : Continuation -> State -> ( Result RuntimeError Value, State )
newMutex cont state =
    let
        ( resultRef, state1 ) =
            newref (VMutex Mutex.new) state
    in
    case resultRef of
        Ok ref ->
            applyCont cont (Ok <| VRef ref) state1

        Err err ->
            applyCont cont (Err err) state1


wait : Ref -> Mutex -> Continuation -> State -> ( Result RuntimeError Value, State )
wait ref m cont state =
    let
        computation =
            \_ -> applyCont cont (Ok VUnit)
    in
    case Mutex.getStatus m of
        Mutex.Open ->
            setref (VMutex <| Mutex.close m) ref state
                |> computation ()

        Mutex.Closed ->
            let
                thread =
                    Thread.new computation
            in
            setref (VMutex <| Mutex.enqueue thread m) ref state
                |> runNextThread


signal : Ref -> Mutex -> Continuation -> State -> ( Result RuntimeError Value, State )
signal ref m cont state =
    let
        computation =
            \_ -> applyCont cont (Ok VUnit)
    in
    case Mutex.getStatus m of
        Mutex.Open ->
            state
                |> computation ()

        Mutex.Closed ->
            case Mutex.dequeue m of
                Just ( thread, newM ) ->
                    state
                        |> setref (VMutex newM) ref
                        |> schedule thread
                        |> computation ()

                Nothing ->
                    state
                        |> setref (VMutex <| Mutex.open m) ref
                        |> computation ()



-- SCHEDULER


schedule : Thread (State -> ( Result RuntimeError Value, State )) -> State -> State
schedule thread (State state) =
    State
        { state
            | scheduler = Scheduler.schedule thread state.scheduler
        }


setFinalAnswer : (State -> ( Result RuntimeError Value, State )) -> State -> State
setFinalAnswer finalAnswer (State state) =
    State
        { state
            | scheduler = Scheduler.setFinalAnswer finalAnswer state.scheduler
        }


isTimeExpired : State -> Bool
isTimeExpired (State { scheduler }) =
    Scheduler.isTimeExpired scheduler


tick : State -> State
tick (State state) =
    State
        { state
            | scheduler = Scheduler.tick state.scheduler
        }


runNextThread : State -> ( Result RuntimeError Value, State )
runNextThread (State state) =
    case Scheduler.runNextThread state.scheduler of
        Just ( f, scheduler ) ->
            let
                state1 =
                    State { state | scheduler = scheduler }
            in
            f state1

        Nothing ->
            ( Err <| UnexpectedError "finalAnswer is missing"
            , State state
            )



-- OUTPUT


printValue : Value -> State -> State
printValue value =
    print <| toString value


print : String -> State -> State
print s (State state) =
    State
        { state
            | output = Output.print s state.output
        }



-- ENV


find : Id -> Env -> State -> ( Result RuntimeError Ref, State )
find name env (State state) =
    let
        findOptions =
            { needle = name
            , toClosure = toClosure
            , store0 = state.store
            }
    in
    case Env.find findOptions env of
        Just (Env.Ref ref) ->
            ( Ok ref
            , State state
            )

        Just (Env.Procedure ref store) ->
            ( Ok ref
            , State { state | store = store }
            )

        Nothing ->
            ( Err <| IdentifierNotFound name
            , State state
            )


toClosure : Id -> Expr -> Env -> Value
toClosure param body savedEnv =
    VProcedure <| Closure param body savedEnv



-- STORE


newref : Value -> State -> ( Result RuntimeError Ref, State )
newref v (State state) =
    let
        ( ref, store ) =
            Store.newref v state.store
    in
    ( Ok ref
    , State { state | store = store }
    )


deref : Ref -> State -> Result RuntimeError Value
deref ref (State { store }) =
    case Store.deref ref store of
        Just v ->
            Ok v

        Nothing ->
            Err <| UnexpectedError <| "reference not found: " ++ Store.refToString ref


setref : Value -> Ref -> State -> State
setref v ref (State state) =
    State { state | store = Store.setref ref v state.store }
