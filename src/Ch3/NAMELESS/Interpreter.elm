module Ch3.NAMELESS.Interpreter exposing (Value(..), run)

import Ch3.NAMELESS.Env as Env
import Ch3.NAMELESS.Translator as T
import Ch3.NAMELESS.Translator.AST as AST exposing (..)
import Ch3.PROC.AST
import Ch3.PROC.Parser as P


type Value
    = VNumber Number
    | VBool Bool
    | VProcedure Procedure


type alias Env =
    Env.Env Value


type Procedure
    = Closure Expr Env


type Type
    = TNumber
    | TBool
    | TProcedure


type Error
    = SyntaxError P.Error
    | RuntimeError RuntimeError


type RuntimeError
    = IdentifierNotFound Ch3.PROC.AST.Id
    | TypeError
        { expected : List Type
        , actual : List Type
        }
    | UnexpectedError String


run : String -> Result Error Value
run input =
    case P.parse input of
        Ok procProgram ->
            case T.translate procProgram of
                Ok namelessProgram ->
                    evalProgram namelessProgram
                        |> Result.mapError RuntimeError

                Err err ->
                    case err of
                        T.IdentifierNotFound name ->
                            Err <| RuntimeError <| IdentifierNotFound name

        Err err ->
            Err <| SyntaxError err


evalProgram : AST.Program -> Result RuntimeError Value
evalProgram (Program expr) =
    evalExpr expr initEnv


initEnv : Env
initEnv =
    --
    -- x = 10, v = 5, i = 1
    --
    Env.empty
        |> Env.extend (VNumber 10)
        |> Env.extend (VNumber 5)
        |> Env.extend (VNumber 1)


evalExpr : Expr -> Env -> Result RuntimeError Value
evalExpr expr env =
    case expr of
        Const n ->
            Ok <| VNumber n

        Var lexAddr ->
            case Env.find lexAddr env of
                Just value ->
                    Ok value

                Nothing ->
                    --
                    -- This should NEVER happen.
                    --
                    Err <| UnexpectedError <| "Lexical address not found: " ++ String.fromInt lexAddr

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

        Let e body ->
            evalExpr e env
                |> Result.andThen
                    (\ve ->
                        evalExpr body (Env.extend ve env)
                    )

        Proc body ->
            Ok <| VProcedure <| Closure body env

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
applyProcedure (Closure body savedEnv) value =
    evalExpr body (Env.extend value savedEnv)


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
