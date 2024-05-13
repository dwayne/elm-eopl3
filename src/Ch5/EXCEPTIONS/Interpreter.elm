module Ch5.EXCEPTIONS.Interpreter exposing (Error(..), RuntimeError(..), Value(..), run)

import Ch5.CONTINUATION_PASSING.Env as Env
import Ch5.EXCEPTIONS.AST as AST exposing (..)
import Ch5.EXCEPTIONS.Parser as P


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
    | UncaughtException Value


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
    | TryCont Id Expr Env Continuation
    | RaiseCont Continuation


evalProgram : AST.Program -> Result RuntimeError Value
evalProgram (Program expr) =
    evalExpr expr initEnv EndCont


initEnv : Env
initEnv =
    Env.empty
        |> Env.extend "x" (VNumber 10)
        |> Env.extend "v" (VNumber 5)
        |> Env.extend "i" (VNumber 1)


evalExpr : Expr -> Env -> Continuation -> Result RuntimeError Value
evalExpr expr env cont =
    case expr of
        Const n ->
            applyCont cont (Ok <| VNumber n)

        Var name ->
            case Env.find name env of
                Just (Env.Value value) ->
                    applyCont cont (Ok value)

                Just (Env.Procedure param body savedEnv) ->
                    applyCont cont (Ok <| VProcedure <| Closure param body savedEnv)

                Nothing ->
                    applyCont cont (Err <| IdentifierNotFound name)

        Diff a b ->
            evalExpr a env (Diff1Cont b env cont)

        Zero a ->
            evalExpr a env (ZeroCont cont)

        If test consequent alternative ->
            evalExpr test env (IfCont consequent alternative env cont)

        Let name e body ->
            evalExpr e env (LetCont name body env cont)

        Proc param body ->
            applyCont cont (Ok <| VProcedure <| Closure param body env)

        Letrec name param procBody letrecBody ->
            evalExpr letrecBody (Env.extendRec name param procBody env) cont

        Call rator rand ->
            evalExpr rator env (RatorCont rand env cont)

        Try e name handlerExpr ->
            evalExpr e env (TryCont name handlerExpr env cont)

        Raise e ->
            evalExpr e env (RaiseCont cont)


applyCont : Continuation -> Result RuntimeError Value -> Result RuntimeError Value
applyCont cont result =
    case cont of
        EndCont ->
            result

        --|> Debug.log "End of computation"
        ZeroCont nextCont ->
            case result of
                Ok va ->
                    applyCont nextCont (evalZero va)

                Err _ ->
                    applyCont nextCont result

        LetCont name body env nextCont ->
            case result of
                Ok ve ->
                    evalExpr body (Env.extend name ve env) nextCont

                Err _ ->
                    applyCont nextCont result

        IfCont consequent alternative env nextCont ->
            case result of
                Ok vTest ->
                    evalIf vTest consequent alternative env nextCont

                Err _ ->
                    applyCont nextCont result

        Diff1Cont b env nextCont ->
            case result of
                Ok va ->
                    evalExpr b env (Diff2Cont va nextCont)

                Err _ ->
                    applyCont nextCont result

        Diff2Cont va nextCont ->
            case result of
                Ok vb ->
                    applyCont nextCont (evalDiff va vb)

                Err _ ->
                    applyCont nextCont result

        RatorCont rand env nextCont ->
            case result of
                Ok vRator ->
                    case toProcedure vRator of
                        Ok f ->
                            evalExpr rand env (RandCont f nextCont)

                        Err err ->
                            applyCont nextCont (Err err)

                Err _ ->
                    applyCont nextCont result

        RandCont f nextCont ->
            case result of
                Ok arg ->
                    applyProcedure f arg nextCont

                Err _ ->
                    applyCont nextCont result

        TryCont _ _ _ nextCont ->
            applyCont nextCont result

        RaiseCont nextCont ->
            case result of
                Ok value ->
                    applyHandler nextCont value

                Err _ ->
                    applyCont nextCont result


applyHandler : Continuation -> Value -> Result RuntimeError Value
applyHandler cont value =
    --
    -- Find the nearest handler to apply.
    --
    case cont of
        TryCont name handlerExpr savedEnv nextCont ->
            evalExpr handlerExpr (Env.extend name value savedEnv) nextCont

        EndCont ->
            Err (UncaughtException value)

        --|> Debug.log "End of computation"
        ZeroCont nextCont ->
            applyHandler nextCont value

        LetCont _ _ _ nextCont ->
            applyHandler nextCont value

        IfCont _ _ _ nextCont ->
            applyHandler nextCont value

        Diff1Cont _ _ nextCont ->
            applyHandler nextCont value

        Diff2Cont _ nextCont ->
            applyHandler nextCont value

        RatorCont _ _ nextCont ->
            applyHandler nextCont value

        RandCont _ nextCont ->
            applyHandler nextCont value

        RaiseCont nextCont ->
            applyHandler nextCont value


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


evalIf : Value -> Expr -> Expr -> Env -> Continuation -> Result RuntimeError Value
evalIf vTest consequent alternative env cont =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env cont

            else
                evalExpr alternative env cont

        _ ->
            applyCont cont <|
                Err <|
                    TypeError
                        { expected = [ TBool ]
                        , actual = [ typeOf vTest ]
                        }


applyProcedure : Procedure -> Value -> Continuation -> Result RuntimeError Value
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
