module Ch5.IMPERATIVE.Interpreter exposing (Value(..), run)

import Ch5.CONTINUATION_PASSING.AST as AST exposing (..)
import Ch5.CONTINUATION_PASSING.Env as Env
import Ch5.CONTINUATION_PASSING.Parser as P


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
    | InternalError String


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


type alias Registers =
    { expr : Expr
    , env : Env
    , cont : Continuation
    , result : Result RuntimeError Value
    }


evalProgram : AST.Program -> Result RuntimeError Value
evalProgram (Program expr) =
    evalExpr
        { expr = expr
        , env = initEnv
        , cont = EndCont
        , result = Err <| InternalError "result not initialized"
        }
        |> .result


initEnv : Env
initEnv =
    Env.empty
        |> Env.extend "x" (VNumber 10)
        |> Env.extend "v" (VNumber 5)
        |> Env.extend "i" (VNumber 1)


evalExpr : Registers -> Registers
evalExpr registers =
    case registers.expr of
        Const n ->
            applyCont
                { registers
                    | result = Ok <| VNumber n
                }

        Var name ->
            case Env.find name registers.env of
                Just (Env.Value value) ->
                    applyCont
                        { registers
                            | result = Ok value
                        }

                Just (Env.Procedure param body savedEnv) ->
                    applyCont
                        { registers
                            | result = Ok <| VProcedure <| Closure param body savedEnv
                        }

                Nothing ->
                    applyCont
                        { registers
                            | result = Err <| IdentifierNotFound name
                        }

        Diff a b ->
            evalExpr
                { registers
                    | expr = a
                    , cont = Diff1Cont b registers.env registers.cont
                }

        Zero a ->
            evalExpr
                { registers
                    | expr = a
                    , cont = ZeroCont registers.cont
                }

        If test consequent alternative ->
            evalExpr
                { registers
                    | expr = test
                    , cont = IfCont consequent alternative registers.env registers.cont
                }

        Let name e body ->
            evalExpr
                { registers
                    | expr = e
                    , cont = LetCont name body registers.env registers.cont
                }

        Proc param body ->
            applyCont
                { registers
                    | result = Ok <| VProcedure <| Closure param body registers.env
                }

        Letrec name param procBody letrecBody ->
            evalExpr
                { registers
                    | expr = letrecBody
                    , env = Env.extendRec name param procBody registers.env
                }

        Call rator rand ->
            evalExpr
                { registers
                    | expr = rator
                    , cont = RatorCont rand registers.env registers.cont
                }


applyCont : Registers -> Registers
applyCont registers =
    case registers.cont of
        EndCont ->
            registers

        ZeroCont nextCont ->
            case registers.result of
                Ok va ->
                    applyCont
                        { registers
                            | cont = nextCont
                            , result = evalZero va
                        }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }

        LetCont name body env nextCont ->
            case registers.result of
                Ok ve ->
                    evalExpr
                        { registers
                            | expr = body
                            , env = Env.extend name ve env
                            , cont = nextCont
                        }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }

        IfCont consequent alternative env nextCont ->
            case registers.result of
                Ok vTest ->
                    evalIf vTest
                        consequent
                        alternative
                        { registers
                            | env = env
                            , cont = nextCont
                        }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }

        Diff1Cont b env nextCont ->
            case registers.result of
                Ok va ->
                    evalExpr
                        { registers
                            | expr = b
                            , env = env
                            , cont = Diff2Cont va nextCont
                        }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }

        Diff2Cont va nextCont ->
            case registers.result of
                Ok vb ->
                    applyCont
                        { registers
                            | cont = nextCont
                            , result = evalDiff va vb
                        }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }

        RatorCont rand env nextCont ->
            case registers.result of
                Ok vRator ->
                    case toProcedure vRator of
                        Ok f ->
                            evalExpr
                                { registers
                                    | expr = rand
                                    , env = env
                                    , cont = RandCont f nextCont
                                }

                        Err err ->
                            applyCont
                                { registers
                                    | cont = nextCont
                                    , result = Err err
                                }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }

        RandCont f nextCont ->
            case registers.result of
                Ok arg ->
                    applyProcedure f
                        arg
                        { registers
                            | cont = nextCont
                        }

                Err _ ->
                    applyCont
                        { registers
                            | cont = nextCont
                        }


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


evalIf : Value -> Expr -> Expr -> Registers -> Registers
evalIf vTest consequent alternative registers =
    case vTest of
        VBool b ->
            if b then
                evalExpr
                    { registers
                        | expr = consequent
                    }

            else
                evalExpr
                    { registers
                        | expr = alternative
                    }

        _ ->
            applyCont
                { registers
                    | result =
                        Err <|
                            TypeError
                                { expected = [ TBool ]
                                , actual = [ typeOf vTest ]
                                }
                }


applyProcedure : Procedure -> Value -> Registers -> Registers
applyProcedure (Closure param body savedEnv) value registers =
    evalExpr
        { registers
            | expr = body
            , env = Env.extend param value savedEnv
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


typeOf : Value -> Type
typeOf v =
    case v of
        VNumber _ ->
            TNumber

        VBool _ ->
            TBool

        VProcedure _ ->
            TProcedure
