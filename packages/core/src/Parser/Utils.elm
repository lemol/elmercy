module Parser.Utils exposing (..)

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


checkFunctionType : String -> (TypeAnnotation.TypeAnnotation -> a) -> Module -> a -> a
checkFunctionType name getter { declarations } default =
    declarations
        |> filterFunctions
        |> List.filter (Tuple.first >> (==) name)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.map getter
        |> Maybe.withDefault default


functionType : Expression.Function -> Maybe TypeAnnotation.TypeAnnotation
functionType f =
    f.signature
        |> Maybe.map Node.value
        |> Maybe.map .typeAnnotation
        |> Maybe.map Node.value


filterFunctions : List Declaration -> List ( String, TypeAnnotation.TypeAnnotation )
filterFunctions =
    let
        filter declaration =
            case declaration of
                Declaration.FunctionDeclaration f ->
                    f.signature
                        |> Maybe.map (Tuple.pair (functionName f))
                        |> Maybe.map (Tuple.mapSecond (Node.value >> .typeAnnotation >> Node.value))

                _ ->
                    Nothing
    in
    List.map filter
        >> List.filterMap identity
