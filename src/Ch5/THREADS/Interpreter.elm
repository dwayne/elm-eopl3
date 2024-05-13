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


evalProgram : AST.Program -> Result RuntimeError Value
evalProgram (Program expr) =
    let
        ( initEnv, initStore ) =
            initState
    in
    evalExpr expr initEnv initStore
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


evalExpr : Expr -> Env -> Store -> ( Result RuntimeError Value, Store )
evalExpr expr env store0 =
    case expr of
        Const n ->
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
            ( result2
            , store2
            )

        Diff a b ->
            let
                ( resultA, store1 ) =
                    evalExpr a env store0
            in
            case resultA of
                Ok va ->
                    let
                        ( resultB, store2 ) =
                            evalExpr b env store1
                    in
                    case resultB of
                        Ok vb ->
                            ( evalDiff va vb
                            , store2
                            )

                        Err _ ->
                            ( resultB
                            , store2
                            )

                Err _ ->
                    ( resultA
                    , store1
                    )

        Zero a ->
            let
                ( resultA, store1 ) =
                    evalExpr a env store0
            in
            case resultA of
                Ok va ->
                    ( evalZero va
                    , store1
                    )

                Err _ ->
                    ( resultA
                    , store1
                    )

        If test consequent alternative ->
            let
                ( resultTest, store1 ) =
                    evalExpr test env store0
            in
            case resultTest of
                Ok vTest ->
                    evalIf vTest consequent alternative env store1

                Err _ ->
                    ( resultTest
                    , store1
                    )

        Let name e body ->
            let
                ( resultE, store1 ) =
                    evalExpr e env store0
            in
            case resultE of
                Ok ve ->
                    let
                        ( resultRef, store2 ) =
                            newref ve store1
                    in
                    case resultRef of
                        Ok ref ->
                            evalExpr body (Env.extend name ref env) store2

                        Err err ->
                            ( Err err
                            , store2
                            )

                Err _ ->
                    ( resultE
                    , store1
                    )

        Proc param body ->
            ( Ok <| VProcedure <| Closure param body env
            , store0
            )

        Letrec procrecs letrecBody ->
            evalExpr letrecBody (Env.extendRec procrecs env) store0

        Call rator rand ->
            let
                ( resultRator, store1 ) =
                    evalExpr rator env store0
            in
            case resultRator of
                Ok vRator ->
                    let
                        resultF =
                            toProcedure vRator
                    in
                    case resultF of
                        Ok f ->
                            let
                                ( resultRand, store2 ) =
                                    evalExpr rand env store1
                            in
                            case resultRand of
                                Ok vRand ->
                                    applyProcedure f vRand store2

                                Err _ ->
                                    ( resultRand
                                    , store2
                                    )

                        Err err ->
                            ( Err err
                            , store1
                            )

                Err _ ->
                    ( resultRator
                    , store1
                    )

        Set name e ->
            let
                ( resultE, store1 ) =
                    evalExpr e env store0
            in
            case resultE of
                Ok ve ->
                    let
                        ( result, store2 ) =
                            find name env store1
                    in
                    case result of
                        Ok ref ->
                            setref ve ref store2

                        Err err ->
                            ( Err err
                            , store2
                            )

                Err _ ->
                    ( resultE
                    , store1
                    )

        Begin firstExpr restExprs ->
            evalExprs (firstExpr :: restExprs) env store0


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


evalIf : Value -> Expr -> Expr -> Env -> Store -> ( Result RuntimeError Value, Store )
evalIf vTest consequent alternative env store =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env store

            else
                evalExpr alternative env store

        _ ->
            ( Err <|
                TypeError
                    { expected = [ TBool ]
                    , actual = [ typeOf vTest ]
                    }
            , store
            )


applyProcedure : Procedure -> Value -> Store -> ( Result RuntimeError Value, Store )
applyProcedure (Closure param body savedEnv) value store0 =
    let
        ( result, store1 ) =
            newref value store0
    in
    case result of
        Ok ref ->
            evalExpr body (Env.extend param ref savedEnv) store1

        Err err ->
            ( Err err
            , store1
            )


evalExprs : List Expr -> Env -> Store -> ( Result RuntimeError Value, Store )
evalExprs exprs env store0 =
    case exprs of
        [ expr ] ->
            evalExpr expr env store0

        expr :: restExprs ->
            let
                ( _, store1 ) =
                    evalExpr expr env store0
            in
            evalExprs restExprs env store1

        [] ->
            --
            -- N.B. This should NEVER happen since the parser
            --      expects begin to have at least one expression.
            --
            ( Err <| UnexpectedError "begin has no expressions"
            , store0
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
