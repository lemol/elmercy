module Main exposing (..)

import Data exposing (..)
import Elm.Interface as Interface
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Node as Node
import Parsing.SimpleHtml as SimpleHtml
import Parsing.SinglePage as SinglePage


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

        mod =
            { declarations = file.declarations |> List.map Node.value
            , interface = Interface.build file_
            }

        exposedPage =
            findValidExposing mod
    in
    { routeName = routeName
    , routePath = routePath
    , exposedPage = exposedPage
    }


findValidExposing : Module -> ExposedPage
findValidExposing mod =
    [ SinglePage.find mod
    , SimpleHtml.find mod
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault None
