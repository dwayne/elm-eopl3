module Ch3.LET.Interpreter exposing (Value(..), run)

import Ch3.LET.AST as AST exposing (..)
import Ch3.LET.Env as Env
import Ch3.LET.Parser as P


type Value
    = VNumber Number
    | VBool Bool


type alias Env =
    Env.Env Id Value


type Type
    = TNumber
    | TBool


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

        Var id ->
            case Env.apply id env of
                Just v ->
                    Ok v

                Nothing ->
                    Err <| IdentifierNotFound id

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

        Let id e body ->
            evalExpr e env
                |> Result.andThen
                    (\ve ->
                        evalExpr body (Env.extend id ve env)
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


typeOf : Value -> Type
typeOf v =
    case v of
        VNumber _ ->
            TNumber

        VBool _ ->
            TBool
