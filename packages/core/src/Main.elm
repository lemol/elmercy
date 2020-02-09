module Main exposing (..)

import Elm.Interface as Interface exposing (Interface)
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation


type ExposedPage
    = None
    | SimpleHtml String


type alias AppPage =
    { routeName : String
    , routePath : String
    , exposedPage : ExposedPage
    }


parsePage : String -> Result String AppPage
parsePage sourceCode =
    Parser.parse sourceCode
        |> Result.map emitAppPage
        |> Result.mapError (always "ERROR")


emitAppPage : RawFile.RawFile -> AppPage
emitAppPage file_ =
    let
        file =
            file_
                |> Processing.process Processing.init

        moduleName =
            RawFile.moduleName file_
                |> String.concat

        routeName =
            moduleName

        routePath =
            if String.startsWith "Pages." moduleName then
                moduleName |> String.dropLeft 6

            else
                "/"

        exposedPage =
            findValidExposing (file.declarations |> List.map Node.value) (Interface.build file_)
    in
    { routeName = routeName
    , routePath = routePath
    , exposedPage = exposedPage
    }


findValidExposing : List Declaration -> Interface -> ExposedPage
findValidExposing items interface =
    [ findSimpleHtml items interface
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault None


findSimpleHtml : List Declaration -> Interface -> Maybe ExposedPage
findSimpleHtml items interface =
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
        items
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

        TypeAnnotation.FunctionTypeAnnotation _ x ->
            isHtmlReturnType t

        _ ->
            False
