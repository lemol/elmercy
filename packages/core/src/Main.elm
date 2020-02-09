module Main exposing (..)

import Dict
import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Exposing as Exposing
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module as Module exposing (Module)
import Elm.Data.Type as Type


type ExposedPage
    = None
    | SimpleHtml String


type alias AppModule =
    { routeName : String
    , routePath : String
    , exposedPage : ExposedPage
    }


type alias ModuleArgs =
    { filePath : String
    , sourceCode : String
    }


parsePage : ModuleArgs -> Result String AppModule
parsePage args =
    let
        file =
            { filePath = args.filePath
            , sourceCode = args.sourceCode
            }
    in
    file
        |> Elm.Compiler.parseModule
        |> Result.andThen Elm.Compiler.desugarOnlyModule
        |> Result.andThen Elm.Compiler.inferModule
        |> Result.map Elm.Compiler.optimizeModule
        |> Result.map (Module.map Typed.unwrap)
        |> Result.map (emitAppModule args)
        |> Result.mapError Elm.Compiler.Error.toString


parseProject : List ModuleArgs -> Result String (List AppModule)
parseProject args =
    let
        project =
            args
                |> List.map
                    (\x ->
                        { filePath = x.filePath
                        , sourceCode = x.sourceCode
                        }
                    )
    in
    project
        |> Elm.Compiler.parseModules
        |> Result.andThen Elm.Compiler.desugarModules
        |> Result.andThen Elm.Compiler.inferModules
        |> Result.map Elm.Compiler.optimizeModules
        |> Result.map (Dict.filter (\k _ -> k == "Main"))
        |> Result.map Dict.values
        |> Result.map (List.map <| Module.map Typed.unwrap)
        |> Result.map (List.map <| emitAppModule { filePath = "", sourceCode = "" })
        |> Result.mapError Elm.Compiler.Error.toString


emitAppModule : ModuleArgs -> Module TypedU.Expr -> AppModule
emitAppModule args m =
    let
        ( routeName, routePath ) =
            buildRoute args m

        exposedPage =
            case m.exposing_ of
                Exposing.ExposingAll ->
                    None

                Exposing.ExposingSome items ->
                    findValidExposing items m
    in
    { routeName = routeName
    , routePath = routePath
    , exposedPage = exposedPage
    }


findValidExposing : List Exposing.ExposedItem -> Module TypedU.Expr -> ExposedPage
findValidExposing items m =
    [ findSimpleHtml items m
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault None


findSimpleHtml : List Exposing.ExposedItem -> Module TypedU.Expr -> Maybe ExposedPage
findSimpleHtml items m =
    let
        checkViewFunction item =
            if item == Exposing.ExposedValue "view" then
                Dict.toList m.declarations
                    |> List.map Tuple.second
                    |> List.map .body
                    |> List.map
                        (\x ->
                            case x of
                                Declaration.Value ( _, Type.UserDefinedType k _ ) ->
                                    k.name

                                _ ->
                                    "NONE"
                        )
                    |> String.concat
                    |> SimpleHtml
                    |> Just

            else
                Nothing
    in
    items
        |> List.map checkViewFunction
        |> List.filterMap identity
        |> List.head


buildRoute : ModuleArgs -> Module TypedU.Expr -> ( String, String )
buildRoute _ m =
    let
        routeName =
            m.name

        routePath =
            if String.startsWith "Pages." m.name then
                m.name
                    |> String.dropLeft 6

            else
                "/"
    in
    ( routeName
    , routePath
    )
