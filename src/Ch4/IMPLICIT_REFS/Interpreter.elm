module Ch4.IMPLICIT_REFS.Interpreter exposing (Value(..), run)

import Ch4.IMPLICIT_REFS.AST as AST exposing (..)
import Ch4.IMPLICIT_REFS.Env as Env
import Ch4.IMPLICIT_REFS.Parser as P
import Lib.Eval as Eval
import Lib.Store as Store exposing (Ref)


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
    let
        ( initEnv, initStore ) =
            initState
    in
    evalExpr expr initEnv
        |> Eval.runEval initStore


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


evalExpr : Expr -> Env -> Eval Value
evalExpr expr env =
    case expr of
        Const n ->
            Eval.succeed <| VNumber n

        Var name ->
            find name env
                |> Eval.andThen deref

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
                |> Eval.andThen newref
                |> Eval.andThen
                    (\ref ->
                        evalExpr body (Env.extend name ref env)
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

        Set name e ->
            evalExpr e env
                |> Eval.andThen
                    (\ve ->
                        find name env
                            |> Eval.andThen (setref ve)
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
    newref value
        |> Eval.andThen
            (\ref ->
                evalExpr body <| Env.extend param ref savedEnv
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


find : Id -> Env -> Eval Ref
find name env =
    Eval.getState
        |> Eval.andThen
            (\store0 ->
                let
                    findOptions =
                        { needle = name
                        , toClosure = toClosure
                        , store0 = store0
                        }
                in
                case Env.find findOptions env of
                    Just (Env.Ref ref) ->
                        Eval.succeed ref

                    Just (Env.Procedure ref store1) ->
                        Eval.setState store1
                            |> Eval.followedBy (Eval.succeed ref)

                    Nothing ->
                        Eval.fail <| IdentifierNotFound name
            )


toClosure : Id -> Expr -> Env -> Value
toClosure param body savedEnv =
    VProcedure <| Closure param body savedEnv



-- STORE


newref : Value -> Eval Ref
newref v =
    Eval.getState
        |> Eval.andThen
            (\store0 ->
                let
                    ( ref, store1 ) =
                        Store.newref v store0
                in
                Eval.setState store1
                    |> Eval.followedBy (Eval.succeed ref)
            )


deref : Ref -> Eval Value
deref ref =
    Eval.getState
        |> Eval.andThen
            (\store ->
                case Store.deref ref store of
                    Just v ->
                        Eval.succeed v

                    Nothing ->
                        Eval.fail <| UnexpectedError <| "reference not found: " ++ Store.refToString ref
            )


setref : Value -> Ref -> Eval Value
setref v ref =
    Eval.getState
        |> Eval.andThen (Eval.setState << Store.setref ref v)
        |> Eval.followedBy unit


unit : Eval Value
unit =
    Eval.succeed VUnit
