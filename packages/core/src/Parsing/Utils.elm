module Parsing.Utils exposing (..)

import Data exposing (..)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation


functionName : Expression.Function -> String
functionName =
    .declaration >> Node.value >> .name >> Node.value


declarationName : Declaration -> String
declarationName declaration =
    case declaration of
        Declaration.FunctionDeclaration x ->
            functionName x

        Declaration.AliasDeclaration x ->
            Node.value x.name

        Declaration.CustomTypeDeclaration x ->
            Node.value x.name

        _ ->
            "<not implemented>"


filterDeclarations : List String -> List Declaration -> List Declaration
filterDeclarations names declarations =
    let
        condition declaration =
            names |> List.member (declarationName declaration)
    in
    declarations
        |> List.filter condition


checkFunctionType : String -> (Maybe TypeAnnotation.TypeAnnotation -> Maybe a) -> Module -> Maybe a
checkFunctionType name getter { declarations } =
    let
        check declaration =
            case declaration of
                Declaration.FunctionDeclaration x ->
                    if functionName x == name then
                        getter (Maybe.map (Node.value >> .typeAnnotation >> Node.value) x.signature)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    declarations
        |> List.map check
        |> List.filterMap identity
        |> List.head
