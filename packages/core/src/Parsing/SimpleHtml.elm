module Parsing.SimpleHtml exposing (find)

import Data exposing (..)
import Elm.Interface as Interface
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAnnotation as TypeAnnotation


find : Module -> Maybe ExposedPage
find { interface, declarations } =
    let
        checkExposingFunction =
            [ "view"
            , "main"
            ]
                |> List.any (\f -> Interface.exposesFunction f interface)

        checkViewFunction item =
            case item of
                Declaration.FunctionDeclaration f ->
                    if ([ "view", "main" ] |> List.member (functionName f)) && (functionType >> Maybe.map isHtmlReturnType >> Maybe.withDefault False) f then
                        Just (SimpleHtml <| functionName f)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    if checkExposingFunction then
        declarations
            |> List.map checkViewFunction
            |> List.filterMap identity
            |> List.head

    else
        Nothing


functionName : Expression.Function -> String
functionName =
    .declaration >> Node.value >> .name >> Node.value


functionType : Expression.Function -> Maybe TypeAnnotation.TypeAnnotation
functionType f =
    f.signature
        |> Maybe.map Node.value
        |> Maybe.map .typeAnnotation
        |> Maybe.map Node.value


isHtmlReturnType : TypeAnnotation.TypeAnnotation -> Bool
isHtmlReturnType t =
    case t of
        TypeAnnotation.Typed (Node.Node _ ( _, "Html" )) _ ->
            True

        TypeAnnotation.FunctionTypeAnnotation _ (Node.Node _ x) ->
            isHtmlReturnType x

        _ ->
            False
