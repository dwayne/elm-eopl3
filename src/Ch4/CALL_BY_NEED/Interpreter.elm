module Ch4.CALL_BY_NEED.Interpreter exposing (Value(..), run)

import Ch4.IMPLICIT_REFS.AST as AST exposing (..)
import Ch4.IMPLICIT_REFS.Env as Env
import Ch4.IMPLICIT_REFS.Parser as P
import Lib.Eval as Eval
import Lib.Store as Store exposing (Ref)


type
    Value
    --
    -- An expressed value.
    --
    = VUnit
    | VNumber Number
    | VBool Bool
    | VProcedure Procedure


type alias Env =
    Env.Env Id Expr


type
    DValue
    --
    -- A denoted value.
    --
    = DValue Value
    | DThunk Thunk


type Thunk
    = Thunk Expr Env


type alias Store =
    Store.Store DValue


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
            Store.newref (DValue <| VNumber 10) store0

        ( vRef, store2 ) =
            Store.newref (DValue <| VNumber 5) store1

        ( iRef, store3 ) =
            Store.newref (DValue <| VNumber 1) store2
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
                |> Eval.andThen
                    (\ref ->
                        deref ref
                            |> Eval.andThen
                                (\dValue ->
                                    case dValue of
                                        DValue v ->
                                            Eval.succeed v

                                        DThunk (Thunk e savedEnv) ->
                                            --
                                            -- 2. The operand is thawed.
                                            --
                                            evalExpr e savedEnv
                                                |> Eval.andThen
                                                    (\v ->
                                                        --
                                                        -- 3. Save the expressed value of the thunk
                                                        --    in the same location, so that the thunk
                                                        --    will not be evaluated again.
                                                        --
                                                        --    This is an instance of a general strategy
                                                        --    called memoization.
                                                        --
                                                        setref v ref
                                                            |> Eval.followedBy (Eval.succeed v)
                                                    )
                                )
                    )

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
                |> Eval.andThen (DValue >> newref)
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
                                    evalOperand rand env
                                        |> Eval.andThen (applyProcedure f)
                                )
                    )

        Set name e ->
            evalExpr e env
                |> Eval.andThen
                    (\ve ->
                        find name env
                            |> Eval.andThen (setref ve)
                            |> Eval.followedBy (Eval.succeed VUnit)
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


evalOperand : Expr -> Env -> Eval Ref
evalOperand expr env =
    case expr of
        Var name ->
            --
            -- This is the key change that changes how parameter-passing works.
            --
            -- Use the reference that the variable denotes. This gives us the
            -- call-by-reference (CBR) instead of the call-by-value (CBV)
            -- semantics.
            --
            find name env

        _ ->
            --
            -- 1. The operand is frozen.
            --
            newref <| DThunk <| Thunk expr env


applyProcedure : Procedure -> Ref -> Eval Value
applyProcedure (Closure param body savedEnv) ref =
    evalExpr body <| Env.extend param ref savedEnv


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


toClosure : Id -> Expr -> Env -> DValue
toClosure param body savedEnv =
    DValue <| VProcedure <| Closure param body savedEnv



-- STORE


newref : DValue -> Eval Ref
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


deref : Ref -> Eval DValue
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


setref : Value -> Ref -> Eval ()
setref v ref =
    Eval.getState
        |> Eval.andThen (Eval.setState << Store.setref ref (DValue v))
