module Ch3.LETREC.Interpreter exposing (Value(..), run)

import Ch3.LETREC.AST as AST exposing (..)
import Ch3.LETREC.Env as Env
import Ch3.LETREC.Parser as P


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

        Err err ->
            Err <| SyntaxError err


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
                                    evalDiff va vb
                                )
                    )

        Zero a ->
            evalExpr a env
                |> Result.andThen
                    (\va ->
                        evalZero va
                    )

        If test consequent alternative ->
            evalExpr test env
                |> Result.andThen
                    (\vTest ->
                        evalIf vTest consequent alternative env
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


evalIf : Value -> Expr -> Expr -> Env -> Result RuntimeError Value
evalIf vTest consequent alternative env =
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


applyProcedure : Procedure -> Value -> Result RuntimeError Value
applyProcedure (Closure param body savedEnv) value =
    evalExpr body (Env.extend param value savedEnv)


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
        VNumber _ ->
            TNumber

        VBool _ ->
            TBool

        VProcedure _ ->
            TProcedure
