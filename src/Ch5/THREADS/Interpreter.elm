module Ch5.THREADS.Interpreter exposing (Value(..), run)

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref)
import Ch5.THREADS.AST as AST exposing (..)
import Ch5.THREADS.Env as Env
import Ch5.THREADS.Parser as P


type Value
    = VUnit
    | VNumber Number
    | VBool Bool
    | VProcedure Procedure


type alias Env =
    Env.Env Id Expr


type alias Store =
    Store.Store Value


type Procedure
    = Closure Id Expr Env


type Type
    = TUnit
    | TNumber
    | TBool
    | TProcedure


type Error
    = SyntaxError P.Error
    | RuntimeError RuntimeError


type RuntimeError
    = IdentifierNotFound Id
    | ReferenceNotFound Ref
    | TypeError
        { expected : List Type
        , actual : List Type
        }
    | UnexpectedError String


run : String -> Result Error Value
run input =
    case P.parse input of
        Ok program ->
            evalProgram program
                |> Result.mapError RuntimeError

        Err err ->
            Err <| SyntaxError err


type Continuation
    = EndCont
    | ZeroCont Continuation
    | LetCont Id Expr Env Continuation
    | IfCont Expr Expr Env Continuation
    | Diff1Cont Expr Env Continuation
    | Diff2Cont Value Continuation
    | RatorCont Expr Env Continuation
    | RandCont Procedure Continuation
    | SetCont Id Env Continuation
    | SequenceCont (List Expr) Env Continuation


evalProgram : AST.Program -> Result RuntimeError Value
evalProgram (Program expr) =
    let
        ( initEnv, initStore ) =
            initState
    in
    evalExpr expr initEnv EndCont initStore
        |> Tuple.first


initState : ( Env, Store )
initState =
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
    , store3
    )


evalExpr : Expr -> Env -> Continuation -> Store -> ( Result RuntimeError Value, Store )
evalExpr expr env cont store0 =
    case expr of
        Const n ->
            applyCont cont
                ( Ok <| VNumber n
                , store0
                )

        Var name ->
            let
                ( result1, store1 ) =
                    find name env store0

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
                , store2
                )

        Diff a b ->
            evalExpr a env (Diff1Cont b env cont) store0

        Zero a ->
            evalExpr a env (ZeroCont cont) store0

        If test consequent alternative ->
            evalExpr test env (IfCont consequent alternative env cont) store0

        Let name e body ->
            evalExpr e env (LetCont name body env cont) store0

        Proc param body ->
            applyCont cont
                ( Ok <| VProcedure <| Closure param body env
                , store0
                )

        Letrec procrecs letrecBody ->
            evalExpr letrecBody (Env.extendRec procrecs env) cont store0

        Call rator rand ->
            evalExpr rator env (RatorCont rand env cont) store0

        Set name e ->
            evalExpr e env (SetCont name env cont) store0

        Begin firstExpr restExprs ->
            evalExpr firstExpr env (SequenceCont restExprs env cont) store0


applyCont : Continuation -> ( Result RuntimeError Value, Store ) -> ( Result RuntimeError Value, Store )
applyCont cont ( result, store0 ) =
    case cont of
        EndCont ->
            ( result
                |> Debug.log "End of computation"
            , store0
            )

        ZeroCont nextCont ->
            case result of
                Ok va ->
                    applyCont nextCont
                        ( evalZero va
                        , store0
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        LetCont name body env nextCont ->
            case result of
                Ok ve ->
                    let
                        ( resultRef, store1 ) =
                            newref ve store0
                    in
                    case resultRef of
                        Ok ref ->
                            evalExpr body (Env.extend name ref env) nextCont store1

                        Err err ->
                            applyCont nextCont
                                ( Err err
                                , store1
                                )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        IfCont consequent alternative env nextCont ->
            case result of
                Ok vTest ->
                    evalIf vTest consequent alternative env nextCont store0

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        Diff1Cont b env nextCont ->
            case result of
                Ok va ->
                    evalExpr b env (Diff2Cont va nextCont) store0

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        Diff2Cont va nextCont ->
            case result of
                Ok vb ->
                    applyCont nextCont
                        ( evalDiff va vb
                        , store0
                        )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
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
                            evalExpr rand env (RandCont f nextCont) store0

                        Err err ->
                            applyCont nextCont
                                ( Err err
                                , store0
                                )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        RandCont f nextCont ->
            case result of
                Ok vRand ->
                    applyProcedure f vRand nextCont store0

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        SetCont name env nextCont ->
            case result of
                Ok ve ->
                    let
                        ( findResult, store1 ) =
                            find name env store0
                    in
                    case findResult of
                        Ok ref ->
                            applyCont nextCont <|
                                setref ve ref store1

                        Err err ->
                            applyCont nextCont
                                ( Err err
                                , store1
                                )

                Err _ ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

        SequenceCont exprs env nextCont ->
            case exprs of
                [] ->
                    applyCont nextCont
                        ( result
                        , store0
                        )

                expr :: restExprs ->
                    evalExpr expr env (SequenceCont restExprs env nextCont) store0


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


evalIf : Value -> Expr -> Expr -> Env -> Continuation -> Store -> ( Result RuntimeError Value, Store )
evalIf vTest consequent alternative env cont store =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env cont store

            else
                evalExpr alternative env cont store

        _ ->
            applyCont cont
                ( Err <|
                    TypeError
                        { expected = [ TBool ]
                        , actual = [ typeOf vTest ]
                        }
                , store
                )


applyProcedure : Procedure -> Value -> Continuation -> Store -> ( Result RuntimeError Value, Store )
applyProcedure (Closure param body savedEnv) value cont store0 =
    let
        ( result, store1 ) =
            newref value store0
    in
    case result of
        Ok ref ->
            evalExpr body (Env.extend param ref savedEnv) cont store1

        Err err ->
            applyCont cont
                ( Err err
                , store1
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


typeOf : Value -> Type
typeOf v =
    case v of
        VUnit ->
            TUnit

        VNumber _ ->
            TNumber

        VBool _ ->
            TBool

        VProcedure _ ->
            TProcedure



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
