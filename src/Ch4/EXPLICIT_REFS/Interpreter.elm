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


evalExpr : Expr -> Env -> Store -> ( Result RuntimeError Value, Store )
evalExpr expr env store0 =
    case expr of
        Const n ->
            ( Ok <| VNumber n
            , store0
            )

        Var name ->
            ( case Env.find name env of
                Just (Env.Value value) ->
                    Ok value

                Just (Env.Procedure param body savedEnv) ->
                    Ok <| VProcedure <| Closure param body savedEnv

                Nothing ->
                    Err <| IdentifierNotFound name
            , store0
            )

        Diff a b ->
            evalExpr a env store0
                |> andThen
                    (\va store1 ->
                        evalExpr b env store1
                            |> andThen
                                (\vb store2 ->
                                    computeDiff va vb store2
                                )
                    )

        Zero a ->
            evalExpr a env store0
                |> andThen
                    (\va store1 ->
                        computeIsZero va store1
                    )

        If test consequent alternative ->
            evalExpr test env store0
                |> andThen
                    (\vTest store1 ->
                        computeIf vTest consequent alternative env store1
                    )

        Let name e body ->
            evalExpr e env store0
                |> andThen
                    (\ve store1 ->
                        evalExpr body (Env.extend name ve env) store1
                    )

        Proc param body ->
            ( Ok <| VProcedure <| Closure param body env
            , store0
            )

        Letrec procrecs letrecBody ->
            evalExpr letrecBody (Env.extendRec procrecs env) store0

        Call rator rand ->
            evalExpr rator env store0
                |> andThen
                    (\vRator store1 ->
                        toProcedure vRator store1
                            |> andThen
                                (\f store2 ->
                                    evalExpr rand env store2
                                        |> andThen
                                            (\arg store3 ->
                                                applyProcedure f arg store3
                                            )
                                )
                    )

        Newref e ->
            evalExpr e env store0
                |> andThen computeNewref

        Deref _ ->
            Debug.todo "Implement deref"

        Setref _ _ ->
            Debug.todo "Implement setref"


computeDiff : Value -> Value -> Store -> ( Result RuntimeError Value, Store )
computeDiff va vb store =
    case ( va, vb ) of
        ( VNumber a, VNumber b ) ->
            ( Ok <| VNumber <| a - b
            , store
            )

        _ ->
            ( Err <|
                TypeError
                    { expected = [ TNumber, TNumber ]
                    , actual = [ typeOf va, typeOf vb ]
                    }
            , store
            )


computeIsZero : Value -> Store -> ( Result RuntimeError Value, Store )
computeIsZero va store =
    case va of
        VNumber n ->
            ( Ok <| VBool <| n == 0
            , store
            )

        _ ->
            ( Err <|
                TypeError
                    { expected = [ TNumber ]
                    , actual = [ typeOf va ]
                    }
            , store
            )


computeIf : Value -> Expr -> Expr -> Env -> Store -> ( Result RuntimeError Value, Store )
computeIf vTest consequent alternative env store =
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


toProcedure : Value -> Store -> ( Result RuntimeError Procedure, Store )
toProcedure v store =
    case v of
        VProcedure f ->
            ( Ok f
            , store
            )

        _ ->
            ( Err <|
                TypeError
                    { expected = [ TProcedure ]
                    , actual = [ typeOf v ]
                    }
            , store
            )


applyProcedure : Procedure -> Value -> Store -> ( Result RuntimeError Value, Store )
applyProcedure (Closure param body savedEnv) value store =
    evalExpr body (Env.extend param value savedEnv) store


computeNewref : Value -> Store -> ( Result RuntimeError Value, Store )
computeNewref value store =
    Store.newref value store
        |> Tuple.mapFirst (Ok << VRef)


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


andThen :
    (a -> Store -> ( Result RuntimeError b, Store ))
    -> ( Result RuntimeError a, Store )
    -> ( Result RuntimeError b, Store )
andThen f ( resultA, store ) =
    case resultA of
        Ok a ->
            f a store

        Err e ->
            ( Err e, store )
