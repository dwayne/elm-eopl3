module Ch4.EXPLICIT_REFS.Interpreter exposing (Value(..), run)

import Ch4.EXPLICIT_REFS.AST as AST exposing (..)
import Ch4.EXPLICIT_REFS.Env as Env
import Ch4.EXPLICIT_REFS.Parser as P
import Lib.Eval as Eval
import Lib.Store as Store exposing (Ref)


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
    | UnexpectedError String


type alias Eval a =
    Eval.Eval Store RuntimeError a


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
        |> Eval.runEval Store.empty


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
            Eval.succeed <| VNumber n

        Var name ->
            case Env.find name env of
                Just (Env.Value value) ->
                    Eval.succeed value

                Just (Env.Procedure param body savedEnv) ->
                    Eval.succeed <| VProcedure <| Closure param body savedEnv

                Nothing ->
                    Eval.fail <| IdentifierNotFound name

        Diff a b ->
            evalExpr a env
                |> Eval.andThen
                    (\va ->
                        evalExpr b env
                            |> Eval.andThen
                                (\vb ->
                                    evalDiff va vb
                                )
                    )

        Zero a ->
            evalExpr a env
                |> Eval.andThen evalZero

        If test consequent alternative ->
            evalExpr test env
                |> Eval.andThen
                    (\vTest ->
                        evalIf vTest consequent alternative env
                    )

        Let name e body ->
            evalExpr e env
                |> Eval.andThen
                    (\ve ->
                        evalExpr body (Env.extend name ve env)
                    )

        Proc param body ->
            Eval.succeed <| VProcedure <| Closure param body env

        Letrec procrecs letrecBody ->
            evalExpr letrecBody (Env.extendRec procrecs env)

        Call rator rand ->
            evalExpr rator env
                |> Eval.andThen
                    (\vRator ->
                        toProcedure vRator
                            |> Eval.andThen
                                (\f ->
                                    evalExpr rand env
                                        |> Eval.andThen (applyProcedure f)
                                )
                    )

        Newref e ->
            evalExpr e env
                |> Eval.andThen evalNewref

        Deref e ->
            evalExpr e env
                |> Eval.andThen toRef
                |> Eval.andThen evalDeref

        Setref leftExpr rightExpr ->
            evalExpr leftExpr env
                |> Eval.andThen toRef
                |> Eval.andThen
                    (\ref ->
                        evalExpr rightExpr env
                            |> Eval.andThen (evalSetref ref)
                    )

        Begin firstExpr restExprs ->
            evalExprs (firstExpr :: restExprs) env


evalDiff : Value -> Value -> Eval Value
evalDiff va vb =
    case ( va, vb ) of
        ( VNumber a, VNumber b ) ->
            Eval.succeed <| VNumber <| a - b

        _ ->
            Eval.fail <|
                TypeError
                    { expected = [ TNumber, TNumber ]
                    , actual = [ typeOf va, typeOf vb ]
                    }


evalZero : Value -> Eval Value
evalZero va =
    case va of
        VNumber n ->
            Eval.succeed <| VBool <| n == 0

        _ ->
            Eval.fail <|
                TypeError
                    { expected = [ TNumber ]
                    , actual = [ typeOf va ]
                    }


evalIf : Value -> Expr -> Expr -> Env -> Eval Value
evalIf vTest consequent alternative env =
    case vTest of
        VBool b ->
            if b then
                evalExpr consequent env

            else
                evalExpr alternative env

        _ ->
            Eval.fail <|
                TypeError
                    { expected = [ TBool ]
                    , actual = [ typeOf vTest ]
                    }


applyProcedure : Procedure -> Value -> Eval Value
applyProcedure (Closure param body savedEnv) value =
    evalExpr body (Env.extend param value savedEnv)


evalNewref : Value -> Eval Value
evalNewref value =
    Eval.getState
        |> Eval.andThen
            (\store0 ->
                let
                    ( ref, store1 ) =
                        Store.newref value store0
                in
                Eval.setState store1
                    |> Eval.followedBy (Eval.succeed <| VRef ref)
            )


evalDeref : Ref -> Eval Value
evalDeref ref =
    Eval.getState
        |> Eval.andThen
            (\store ->
                case Store.deref ref store of
                    Just value ->
                        Eval.succeed value

                    Nothing ->
                        Eval.fail <| ReferenceNotFound ref
            )


evalSetref : Ref -> Value -> Eval Value
evalSetref ref value =
    Eval.getState
        |> Eval.andThen
            (\store ->
                Eval.setState (Store.setref ref value store)
                    |> Eval.followedBy (Eval.succeed <| VRef ref)
            )


evalExprs : List Expr -> Env -> Eval Value
evalExprs exprs env =
    case exprs of
        [ expr ] ->
            evalExpr expr env

        expr :: restExprs ->
            evalExpr expr env
                |> Eval.followedBy (evalExprs restExprs env)

        [] ->
            --
            -- N.B. This should NEVER happen since the parser
            --      expects begin to have at least one expression.
            --
            Eval.fail <| UnexpectedError "begin has no expressions"


toProcedure : Value -> Eval Procedure
toProcedure v =
    case v of
        VProcedure f ->
            Eval.succeed f

        _ ->
            Eval.fail <|
                TypeError
                    { expected = [ TProcedure ]
                    , actual = [ typeOf v ]
                    }


toRef : Value -> Eval Ref
toRef v =
    case v of
        VRef ref ->
            Eval.succeed ref

        _ ->
            Eval.fail <|
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
