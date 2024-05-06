module Ch4.MUTABLE_PAIRS.Interpreter exposing (Value(..), run)

import Ch4.EXPLICIT_REFS.Store as Store exposing (Ref)
import Ch4.MUTABLE_PAIRS.AST as AST exposing (..)
import Ch4.IMPLICIT_REFS.Env as Env
import Ch4.MUTABLE_PAIRS.Parser as P


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
            succeed <| VNumber n

        Var name ->
            find name env
                |> andThen deref

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
                |> andThen newref
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
                                    evalExpr rand env
                                        |> andThen (applyProcedure f)
                                )
                    )

        Set name e ->
            evalExpr e env
                |> andThen
                    (\ve ->
                        find name env
                            |> andThen (setref ve)
                    )

        Begin firstExpr restExprs ->
            evalExprs (firstExpr :: restExprs) env

        Newpair e1 e2 ->
            Debug.todo "Evaluate Newpair"

        Left e ->
            Debug.todo "Evaluate Left"

        Right e ->
            Debug.todo "Evaluate Right"

        Setleft e1 e2 ->
            Debug.todo "Evaluate Setleft"

        Setright e1 e2 ->
            Debug.todo "Evaluate Setright"


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


applyProcedure : Procedure -> Value -> Eval Value
applyProcedure (Closure param body savedEnv) value =
    newref value
        |> andThen
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


toClosure : Id -> Expr -> Env -> Value
toClosure param body savedEnv =
    VProcedure <| Closure param body savedEnv



-- STORE


newref : Value -> Eval Ref
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


deref : Ref -> Eval Value
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


setref : Value -> Ref -> Eval Value
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
