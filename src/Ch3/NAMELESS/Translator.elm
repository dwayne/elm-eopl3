module Ch3.NAMELESS.Translator exposing (Error(..), translate)

import Ch3.NAMELESS.Translator.AST as AST
import Ch3.NAMELESS.Translator.Env as Env
import Ch3.PROC.AST


type alias Env =
    Env.Env Ch3.PROC.AST.Id


type Error
    = IdentifierNotFound Ch3.PROC.AST.Id


translate : Ch3.PROC.AST.Program -> Result Error AST.Program
translate (Ch3.PROC.AST.Program expr) =
    translateExpr expr initEnv
        |> Result.map AST.Program


initEnv : Env
initEnv =
    Env.empty
        |> Env.extend "x"
        |> Env.extend "v"
        |> Env.extend "i"


translateExpr : Ch3.PROC.AST.Expr -> Env -> Result Error AST.Expr
translateExpr expr env =
    case expr of
        Ch3.PROC.AST.Const n ->
            Ok <| AST.Const n

        Ch3.PROC.AST.Var name ->
            case Env.find name env of
                Just lexAddr ->
                    Ok <| AST.Var lexAddr

                Nothing ->
                    Err <| IdentifierNotFound name

        Ch3.PROC.AST.Diff a b ->
            translateExpr a env
                |> Result.andThen
                    (\na ->
                        translateExpr b env
                            |> Result.map
                                (\nb ->
                                    AST.Diff na nb
                                )
                    )

        Ch3.PROC.AST.Zero a ->
            translateExpr a env
                |> Result.map
                    (\na ->
                        AST.Zero na
                    )

        Ch3.PROC.AST.If test consequent alternative ->
            translateExpr test env
                |> Result.andThen
                    (\nTest ->
                        translateExpr consequent env
                            |> Result.andThen
                                (\nConsequent ->
                                    translateExpr alternative env
                                        |> Result.map
                                            (\nAlternative ->
                                                AST.If nTest nConsequent nAlternative
                                            )
                                )
                    )

        Ch3.PROC.AST.Let name e body ->
            translateExpr e env
                |> Result.andThen
                    (\ne ->
                        translateExpr body (Env.extend name env)
                            |> Result.map
                                (\nBody ->
                                    AST.Let ne nBody
                                )
                    )

        Ch3.PROC.AST.Proc param body ->
            translateExpr body (Env.extend param env)
                |> Result.map
                    (\nBody ->
                        AST.Proc nBody
                    )

        Ch3.PROC.AST.Call rator rand ->
            translateExpr rator env
                |> Result.andThen
                    (\nRator ->
                        translateExpr rand env
                            |> Result.map
                                (\nRand ->
                                    AST.Call nRator nRand
                                )
                    )
