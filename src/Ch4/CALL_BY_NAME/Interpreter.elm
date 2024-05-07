module Ch4.CALL_BY_NAME.Interpreter exposing (Value(..), run)

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref)
import Ch4.IMPLICIT_REFS.AST as AST exposing (..)
import Ch4.IMPLICIT_REFS.Env as Env
import Ch4.IMPLICIT_REFS.Parser as P


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
        |> runEval initStore


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
            succeed <| VNumber n

        Var name ->
            find name env
                |> andThen deref
                |> andThen
                    (\dValue ->
                        case dValue of
                            DValue v ->
                                succeed v

                            DThunk (Thunk e savedEnv) ->
                                --
                                -- 2. The operand is thawed.
                                --
                                evalExpr e savedEnv
                    )

        Diff a b ->
            evalExpr a env
                |> andThen
                    (\va ->
                        evalExpr b env
                            |> andThen
                                (\vb ->
                                    evalDiff va vb
                                )
                    )

        Zero a ->
            evalExpr a env
                |> andThen evalZero

        If test consequent alternative ->
            evalExpr test env
                |> andThen
                    (\vTest ->
                        evalIf vTest consequent alternative env
                    )

        Let name e body ->
            evalExpr e env
                |> andThen (DValue >> newref)
                |> andThen
                    (\ref ->
                        evalExpr body (Env.extend name ref env)
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
                                    evalOperand rand env
                                        |> andThen (applyProcedure f)
                                )
                    )

        Set name e ->
            evalExpr e env
                |> andThen
                    (\ve ->
                        find name env
                            |> andThen (setref <| DValue ve)
                    )

        Begin firstExpr restExprs ->
            evalExprs (firstExpr :: restExprs) env


evalDiff : Value -> Value -> Eval Value
evalDiff va vb =
    case ( va, vb ) of
        ( VNumber a, VNumber b ) ->
            succeed <| VNumber <| a - b

        _ ->
            fail <|
                TypeError
                    { expected = [ TNumber, TNumber ]
                    , actual = [ typeOf va, typeOf vb ]
                    }


evalZero : Value -> Eval Value
evalZero va =
    case va of
        VNumber n ->
            succeed <| VBool <| n == 0

        _ ->
            fail <|
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
            fail <|
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
                |> followedBy (evalExprs restExprs env)

        [] ->
            --
            -- N.B. This should NEVER happen since the parser
            --      expects begin to have at least one expression.
            --
            fail <| UnexpectedError "begin has no expressions"


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
    getStore
        |> andThen
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
                        succeed ref

                    Just (Env.Procedure ref store1) ->
                        setStore store1
                            |> followedBy (succeed ref)

                    Nothing ->
                        fail <| IdentifierNotFound name
            )


toClosure : Id -> Expr -> Env -> DValue
toClosure param body savedEnv =
    DValue <| VProcedure <| Closure param body savedEnv



-- STORE


newref : DValue -> Eval Ref
newref v =
    getStore
        |> andThen
            (\store0 ->
                let
                    ( ref, store1 ) =
                        Store.newref v store0
                in
                setStore store1
                    |> followedBy (succeed ref)
            )


deref : Ref -> Eval DValue
deref ref =
    getStore
        |> andThen
            (\store ->
                case Store.deref ref store of
                    Just v ->
                        succeed v

                    Nothing ->
                        fail <| UnexpectedError <| "reference not found: " ++ Store.refToString ref
            )


setref : DValue -> Ref -> Eval Value
setref v ref =
    getStore
        |> andThen (setStore << Store.setref ref v)
        |> followedBy unit


unit : Eval Value
unit =
    succeed VUnit



-- EVAL


type alias Eval a =
    Store -> ( Result RuntimeError a, Store )


runEval : Store -> Eval a -> Result RuntimeError a
runEval store eval =
    Tuple.first <| eval store


succeed : a -> Eval a
succeed a =
    \store0 ->
        ( Ok a
        , store0
        )


fail : RuntimeError -> Eval a
fail err =
    \store0 ->
        ( Err err
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

            Err err ->
                ( Err err, store1 )


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

            Err err ->
                ( Err err, store1 )


followedBy : Eval b -> Eval a -> Eval b
followedBy evalB evalA =
    evalA
        |> andThen (\_ -> evalB)
