module Ch5.THREADS.Interpreter exposing (State, Value(..), run)

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref)
import Ch5.THREADS.AST as AST exposing (..)
import Ch5.THREADS.Env as Env
import Ch5.THREADS.Output as Output exposing (Output)
import Ch5.THREADS.Parser as P



--
-- TODO:
--
-- [x] Start with IMPLICIT-REFS
-- [x] Remove Eval and implement the store-passing interpreter explicitly
-- [x] Refactor the interpreter to use continuations
-- [x] Add cons, car, cdr, null?, emptylist, list
-- [x] Add print
-- [x] Implement a queue data structure
-- [x] Represent a thread
-- [x] Add a scheduler
-- [x] Add spawn syntax
-- [ ] Implement THREADS
--


type Value
    = VUnit
    | VNumber Number
    | VBool Bool
    | VList (List Value)
    | VProcedure Procedure


type alias Env =
    Env.Env Id Expr


type alias Store =
    Store.Store Value


type alias State =
    { store : Store
    , output : Output
    }


type Procedure
    = Closure Id Expr Env


type Type
    = TUnit
    | TNumber
    | TBool
    | TList
    | TProcedure


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


run : String -> ( Result Error Value, State )
run input =
    let
        ( initEnv, initState ) =
            initEnvAndState
    in
    case P.parse input of
        Ok program ->
            evalProgram initEnv initState program
                |> Tuple.mapFirst (Result.mapError RuntimeError)

        Err err ->
            ( Err <| SyntaxError err
            , initState
            )


initEnvAndState : ( Env, State )
initEnvAndState =
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
    , State store3 Output.empty
    )


type Continuation
    = EndCont
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


evalProgram : Env -> State -> AST.Program -> ( Result RuntimeError Value, State )
evalProgram env state (Program expr) =
    evalExpr expr env EndCont state


evalExpr : Expr -> Env -> Continuation -> State -> ( Result RuntimeError Value, State )
evalExpr expr env cont state =
    case expr of
        Const n ->
            applyCont cont
                ( Ok <| VNumber n
                , state
                )

        Var name ->
            let
                ( result1, store1 ) =
                    find name env state.store

                ( result2, store2 ) =
                    case result1 of
                        Ok ref ->
                            deref ref store1

                        Err err ->
                            ( Err err
                            , store1
                            )
            in
            applyCont cont
                ( result2
                , { state | store = store2 }
                )

        Diff a b ->
            evalExpr a env (Diff1Cont b env cont) state

        Zero a ->
            evalExpr a env (ZeroCont cont) state

        Cons a b ->
            evalExpr a env (Cons1Cont b env cont) state

        Car a ->
            evalExpr a env (CarCont cont) state

        Cdr a ->
            evalExpr a env (CdrCont cont) state

        Null a ->
            evalExpr a env (NullCont cont) state

        EmptyList ->
            applyCont cont
                ( Ok <| VList []
                , state
                )

        List exprs ->
            case exprs of
                [] ->
                    applyCont cont
                        ( Ok <| VList []
                        , state
                        )

                firstExpr :: restExprs ->
                    evalExpr firstExpr env (ListCont [] restExprs env cont) state

        If test consequent alternative ->
            evalExpr test env (IfCont consequent alternative env cont) state

        Let name e body ->
            evalExpr e env (LetCont name body env cont) state

        Proc param body ->
            applyCont cont
                ( Ok <| VProcedure <| Closure param body env
                , state
                )

        Letrec procrecs letrecBody ->
            evalExpr letrecBody (Env.extendRec procrecs env) cont state

        Call rator rand ->
            evalExpr rator env (RatorCont rand env cont) state

        Set name e ->
            evalExpr e env (SetCont name env cont) state

        Begin firstExpr restExprs ->
            evalExpr firstExpr env (SequenceCont restExprs env cont) state

        Print e ->
            evalExpr e env (PrintCont cont) state

        Spawn e ->
            Debug.todo "Implement Spawn"


applyCont : Continuation -> ( Result RuntimeError Value, State ) -> ( Result RuntimeError Value, State )
applyCont cont ( result, state ) =
    case cont of
        EndCont ->
            ( result
                |> Debug.log "End of computation"
            , state
            )

        ZeroCont nextCont ->
            case result of
                Ok va ->
                    applyCont nextCont
                        ( evalZero va
                        , state
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        LetCont name body env nextCont ->
            case result of
                Ok ve ->
                    let
                        ( resultRef, store1 ) =
                            newref ve state.store

                        newState =
                            { state | store = store1 }
                    in
                    case resultRef of
                        Ok ref ->
                            evalExpr body (Env.extend name ref env) nextCont newState

                        Err err ->
                            applyCont nextCont
                                ( Err err
                                , newState
                                )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        IfCont consequent alternative env nextCont ->
            case result of
                Ok vTest ->
                    evalIf vTest consequent alternative env nextCont state

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        Diff1Cont b env nextCont ->
            case result of
                Ok va ->
                    evalExpr b env (Diff2Cont va nextCont) state

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        Diff2Cont va nextCont ->
            case result of
                Ok vb ->
                    applyCont nextCont
                        ( evalDiff va vb
                        , state
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        Cons1Cont b env nextCont ->
            case result of
                Ok va ->
                    evalExpr b env (Cons2Cont va nextCont) state

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        Cons2Cont va nextCont ->
            case result of
                Ok vb ->
                    applyCont nextCont
                        ( evalCons va vb
                        , state
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        CarCont nextCont ->
            case result of
                Ok va ->
                    applyCont nextCont
                        ( evalCar va
                        , state
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        CdrCont nextCont ->
            case result of
                Ok va ->
                    applyCont nextCont
                        ( evalCdr va
                        , state
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        NullCont nextCont ->
            case result of
                Ok va ->
                    applyCont nextCont
                        ( evalNull va
                        , state
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        ListCont prevRevValues exprs env nextCont ->
            case result of
                Ok value ->
                    let
                        revValues =
                            value :: prevRevValues
                    in
                    case exprs of
                        [] ->
                            applyCont nextCont
                                ( Ok <| VList <| List.reverse revValues
                                , state
                                )

                        firstExpr :: restExprs ->
                            evalExpr firstExpr env (ListCont revValues restExprs env nextCont) state

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        RatorCont rand env nextCont ->
            case result of
                Ok vRator ->
                    let
                        resultF =
                            toProcedure vRator
                    in
                    case resultF of
                        Ok f ->
                            evalExpr rand env (RandCont f nextCont) state

                        Err err ->
                            applyCont nextCont
                                ( Err err
                                , state
                                )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        RandCont f nextCont ->
            case result of
                Ok vRand ->
                    applyProcedure f vRand nextCont state

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        SetCont name env nextCont ->
            case result of
                Ok ve ->
                    let
                        ( findResult, store1 ) =
                            find name env state.store
                    in
                    case findResult of
                        Ok ref ->
                            setref ve ref store1
                                |> Tuple.mapSecond (\store2 -> { state | store = store2 })
                                |> applyCont nextCont

                        Err err ->
                            applyCont nextCont
                                ( Err err
                                , { state | store = store1 }
                                )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )

        SequenceCont exprs env nextCont ->
            case exprs of
                [] ->
                    applyCont nextCont
                        ( result
                        , state
                        )

                expr :: restExprs ->
                    evalExpr expr env (SequenceCont restExprs env nextCont) state

        PrintCont nextCont ->
            case result of
                Ok value ->
                    let
                        output1 =
                            Output.print (toString value) state.output
                    in
                    applyCont nextCont
                        ( Ok VUnit
                        , { state | output = output1 }
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , state
                        )


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


evalIf : Value -> Expr -> Expr -> Env -> Continuation -> State -> ( Result RuntimeError Value, State )
evalIf vTest consequent alternative env cont state =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env cont state

            else
                evalExpr alternative env cont state

        _ ->
            applyCont cont
                ( Err <|
                    TypeError
                        { expected = [ TBool ]
                        , actual = [ typeOf vTest ]
                        }
                , state
                )


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
        ( result, store1 ) =
            newref value state.store

        newState =
            { state | store = store1 }
    in
    case result of
        Ok ref ->
            evalExpr body (Env.extend param ref savedEnv) cont newState

        Err err ->
            applyCont cont
                ( Err err
                , newState
                )


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



-- ENV


find : Id -> Env -> Store -> ( Result RuntimeError Ref, Store )
find name env store0 =
    let
        findOptions =
            { needle = name
            , toClosure = toClosure
            , store0 = store0
            }
    in
    case Env.find findOptions env of
        Just (Env.Ref ref) ->
            ( Ok ref
            , store0
            )

        Just (Env.Procedure ref store1) ->
            ( Ok ref
            , store1
            )

        Nothing ->
            ( Err <| IdentifierNotFound name
            , store0
            )


toClosure : Id -> Expr -> Env -> Value
toClosure param body savedEnv =
    VProcedure <| Closure param body savedEnv



-- STORE


newref : Value -> Store -> ( Result RuntimeError Ref, Store )
newref v store0 =
    let
        ( ref, store1 ) =
            Store.newref v store0
    in
    ( Ok ref
    , store1
    )


deref : Ref -> Store -> ( Result RuntimeError Value, Store )
deref ref store =
    case Store.deref ref store of
        Just v ->
            ( Ok v
            , store
            )

        Nothing ->
            ( Err <| UnexpectedError <| "reference not found: " ++ Store.refToString ref
            , store
            )


setref : Value -> Ref -> Store -> ( Result RuntimeError Value, Store )
setref v ref store =
    ( Ok VUnit
    , Store.setref ref v store
    )
