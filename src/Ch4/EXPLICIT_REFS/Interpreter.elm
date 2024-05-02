module Ch4.EXPLICIT_REFS.Interpreter exposing (Value(..), run)

import Ch4.EXPLICIT_REFS.AST as AST exposing (..)
import Ch4.EXPLICIT_REFS.Env as Env
import Ch4.EXPLICIT_REFS.Parser as P
import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref)


type Value
    = VNumber Number
    | VBool Bool
    | VProcedure Procedure
    | VRef Ref


type alias Env =
    Env.Env Id Value Expr


type alias Store =
    Store.Store Value


type Procedure
    = Closure Id Expr Env


type Type
    = TNumber
    | TBool
    | TProcedure
    | TRef


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


run : String -> Result Error Value
run input =
    case P.parse input of
        Ok program ->
            evalProgram program
                |> Result.mapError RuntimeError

        Err e ->
            Err <| SyntaxError e


evalProgram : AST.Program -> Result RuntimeError Value
evalProgram (Program expr) =
    evalExpr expr initEnv Store.empty
        |> Tuple.first


initEnv : Env
initEnv =
    Env.empty
        |> Env.extend "x" (VNumber 10)
        |> Env.extend "v" (VNumber 5)
        |> Env.extend "i" (VNumber 1)


evalExpr : Expr -> Env -> Eval Value
evalExpr expr env =
    case expr of
        Const n ->
            succeed <| VNumber n

        Var name ->
            case Env.find name env of
                Just (Env.Value value) ->
                    succeed value

                Just (Env.Procedure param body savedEnv) ->
                    succeed <| VProcedure <| Closure param body savedEnv

                Nothing ->
                    fail <| IdentifierNotFound name

        Diff a b ->
            evalExpr a env
                |> andThen
                    (\va ->
                        evalExpr b env
                            |> andThen
                                (\vb ->
                                    computeDiff va vb
                                )
                    )

        Zero a ->
            evalExpr a env
                |> andThen computeIsZero

        If test consequent alternative ->
            evalExpr test env
                |> andThen
                    (\vTest ->
                        computeIf vTest consequent alternative env
                    )

        Let name e body ->
            evalExpr e env
                |> andThen
                    (\ve ->
                        evalExpr body (Env.extend name ve env)
                    )

        Proc param body ->
            succeed <| VProcedure <| Closure param body env

        Letrec procrecs letrecBody ->
            evalExpr letrecBody (Env.extendRec procrecs env)

        Call rator rand ->
            evalExpr rator env
                |> andThen
                    (\vRator ->
                        toProcedure vRator
                            |> andThen
                                (\f ->
                                    evalExpr rand env
                                        |> andThen (applyProcedure f)
                                )
                    )

        Newref e ->
            evalExpr e env
                |> andThen computeNewref

        Deref e ->
            evalExpr e env
                |> andThen toRef
                |> andThen computeDeref

        Setref le re ->
            evalExpr le env
                |> andThen toRef
                |> andThen
                    (\ref ->
                        evalExpr re env
                            |> andThen (computeSetRef ref)
                    )


computeDiff : Value -> Value -> Eval Value
computeDiff va vb =
    case ( va, vb ) of
        ( VNumber a, VNumber b ) ->
            succeed <| VNumber <| a - b

        _ ->
            fail <|
                TypeError
                    { expected = [ TNumber, TNumber ]
                    , actual = [ typeOf va, typeOf vb ]
                    }


computeIsZero : Value -> Eval Value
computeIsZero va =
    case va of
        VNumber n ->
            succeed <| VBool <| n == 0

        _ ->
            fail <|
                TypeError
                    { expected = [ TNumber ]
                    , actual = [ typeOf va ]
                    }


computeIf : Value -> Expr -> Expr -> Env -> Eval Value
computeIf vTest consequent alternative env =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env

            else
                evalExpr alternative env

        _ ->
            fail <|
                TypeError
                    { expected = [ TBool ]
                    , actual = [ typeOf vTest ]
                    }


applyProcedure : Procedure -> Value -> Eval Value
applyProcedure (Closure param body savedEnv) value =
    evalExpr body (Env.extend param value savedEnv)


computeNewref : Value -> Eval Value
computeNewref value =
    getStore
        |> andThen
            (\store0 ->
                let
                    ( ref, store1 ) =
                        Store.newref value store0
                in
                setStore store1
                    |> followedBy (succeed <| VRef ref)
            )


computeDeref : Ref -> Eval Value
computeDeref ref =
    getStore
        |> andThen
            (\store ->
                case Store.deref ref store of
                    Just value ->
                        succeed value

                    Nothing ->
                        fail <| ReferenceNotFound ref
            )


computeSetRef : Ref -> Value -> Eval Value
computeSetRef ref value =
    getStore
        |> andThen
            (\store ->
                setStore (Store.setref ref value store)
                    |> followedBy (succeed <| VRef ref)
            )


toProcedure : Value -> Eval Procedure
toProcedure v =
    case v of
        VProcedure f ->
            succeed f

        _ ->
            fail <|
                TypeError
                    { expected = [ TProcedure ]
                    , actual = [ typeOf v ]
                    }


toRef : Value -> Eval Ref
toRef v =
    case v of
        VRef ref ->
            succeed ref

        _ ->
            fail <|
                TypeError
                    { expected = [ TRef ]
                    , actual = [ typeOf v ]
                    }


typeOf : Value -> Type
typeOf v =
    case v of
        VNumber _ ->
            TNumber

        VBool _ ->
            TBool

        VProcedure _ ->
            TProcedure

        VRef _ ->
            TRef



-- EVAL


type alias Eval a =
    Store -> ( Result RuntimeError a, Store )


succeed : a -> Eval a
succeed a =
    \store0 ->
        ( Ok a
        , store0
        )


fail : RuntimeError -> Eval a
fail e =
    \store0 ->
        ( Err e
        , store0
        )


getStore : Eval Store
getStore =
    \store0 ->
        ( Ok store0
        , store0
        )


setStore : Store -> Eval ()
setStore newStore =
    \_ ->
        ( Ok ()
        , newStore
        )


map : (a -> b) -> Eval a -> Eval b
map f evalA =
    \store0 ->
        let
            ( resultA, store1 ) =
                evalA store0
        in
        case resultA of
            Ok a ->
                ( Ok <| f a, store1 )

            Err e ->
                ( Err e, store1 )


andThen : (a -> Eval b) -> Eval a -> Eval b
andThen f evalA =
    \store0 ->
        let
            ( resultA, store1 ) =
                evalA store0
        in
        case resultA of
            Ok a ->
                let
                    evalB =
                        f a
                in
                evalB store1

            Err e ->
                ( Err e, store1 )


followedBy : Eval b -> Eval a -> Eval b
followedBy evalB evalA =
    evalA
        |> andThen (\_ -> evalB)
