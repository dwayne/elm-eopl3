module Ch4.EXPLICIT_REFS.Interpreter exposing (Value(..), run)

import Ch4.EXPLICIT_REFS.AST as AST exposing (..)
import Ch4.EXPLICIT_REFS.Env as Env
import Ch4.EXPLICIT_REFS.Parser as P


type Value
    = VNumber Number
    | VBool Bool
    | VProcedure Procedure


type alias Env =
    Env.Env Id Value Expr


type Procedure
    = Closure Id Expr Env


type Type
    = TNumber
    | TBool
    | TProcedure


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
    evalExpr expr initEnv


initEnv : Env
initEnv =
    Env.empty
        |> Env.extend "x" (VNumber 10)
        |> Env.extend "v" (VNumber 5)
        |> Env.extend "i" (VNumber 1)


evalExpr : Expr -> Env -> Result RuntimeError Value
evalExpr expr env =
    case expr of
        Const n ->
            Ok <| VNumber n

        Var name ->
            case Env.find name env of
                Just (Env.Value value) ->
                    Ok value

                Just (Env.Procedure param body savedEnv) ->
                    Ok <| VProcedure <| Closure param body savedEnv

                Nothing ->
                    Err <| IdentifierNotFound name

        Diff a b ->
            evalExpr a env
                |> Result.andThen
                    (\va ->
                        evalExpr b env
                            |> Result.andThen
                                (\vb ->
                                    computeDiff va vb
                                )
                    )

        Zero a ->
            evalExpr a env
                |> Result.andThen
                    (\va ->
                        computeIsZero va
                    )

        If test consequent alternative ->
            evalExpr test env
                |> Result.andThen
                    (\vTest ->
                        computeIf vTest consequent alternative env
                    )

        Let name e body ->
            evalExpr e env
                |> Result.andThen
                    (\ve ->
                        evalExpr body (Env.extend name ve env)
                    )

        Proc param body ->
            Ok <| VProcedure <| Closure param body env

        Letrec name param procBody letrecBody ->
            evalExpr letrecBody (Env.extendRec name param procBody env)

        Call rator rand ->
            evalExpr rator env
                |> Result.andThen
                    (\vRator ->
                        toProcedure vRator
                            |> Result.andThen
                                (\f ->
                                    evalExpr rand env
                                        |> Result.andThen
                                            (\arg ->
                                                applyProcedure f arg
                                            )
                                )
                    )


computeDiff : Value -> Value -> Result RuntimeError Value
computeDiff va vb =
    case ( va, vb ) of
        ( VNumber a, VNumber b ) ->
            Ok <| VNumber <| a - b

        _ ->
            Err <|
                TypeError
                    { expected = [ TNumber, TNumber ]
                    , actual = [ typeOf va, typeOf vb ]
                    }


computeIsZero : Value -> Result RuntimeError Value
computeIsZero va =
    case va of
        VNumber n ->
            Ok <| VBool <| n == 0

        _ ->
            Err <|
                TypeError
                    { expected = [ TNumber ]
                    , actual = [ typeOf va ]
                    }


computeIf : Value -> Expr -> Expr -> Env -> Result RuntimeError Value
computeIf vTest consequent alternative env =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env

            else
                evalExpr alternative env

        _ ->
            Err <|
                TypeError
                    { expected = [ TBool ]
                    , actual = [ typeOf vTest ]
                    }


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


applyProcedure : Procedure -> Value -> Result RuntimeError Value
applyProcedure (Closure param body savedEnv) value =
    evalExpr body (Env.extend param value savedEnv)


typeOf : Value -> Type
typeOf v =
    case v of
        VNumber _ ->
            TNumber

        VBool _ ->
            TBool

        VProcedure _ ->
            TProcedure
