module Main exposing (..)

import Elm.AST.Typed as Typed
import Elm.AST.Typed.Unwrapped as TypedU
import Elm.Compiler
import Elm.Compiler.Error exposing (Error)
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.Module as Module exposing (Module)


type ExposedPage =
    ViewHtml


type alias AppModule =
    { routeName: String
    , routePath: String
    , exposedType: ExposedType
    }


type ModuleArgs =
    { filePath: String
    , sourceCode: String
    }


parsePage : ModuleArgs -> Maybe AppModule
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


emitAppModule : ModuleArgs -> Module TypedU.Expr -> AppModule
emitAppModule args m =
    let
        (routeName, routePath) =
            buildRoute args
    in
    { routeName = routeName
    , routePath = routePath
    }



buildRoute : ModuleArgs -> ( String, String )
buildRoute args =
    let
        routeName =
            args.filePath
            |> String.split '/'
            |> String.join ""

        routePath =
            ""
    in
    ( routeName
    , routePath
    )