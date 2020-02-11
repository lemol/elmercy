module Parser exposing (parseNextSource)

import Data exposing (App(..), AppPage, AppType(..), Module)
import Elm.Interface as Interface
import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Node as Node
import Parser.MultiplePages as MultiplePages
import Parser.SinglePage as SinglePage


parseNextSource : AppType -> App -> String -> Result String App
parseNextSource appType app source =
    case appType of
        SinglePageAppType ->
            parseSinglePageApp source

        MulitplePagesAppType ->
            let
                list =
                    case app of
                        MulitplePages x ->
                            x

                        _ ->
                            []
            in
            parseMultiplePagesApp list source

        _ ->
            Err "<not implemented yet>"


parseSinglePageApp : String -> Result String App
parseSinglePageApp sourceCode =
    Elm.Parser.parse sourceCode
        |> Result.map emitSiglePageApp
        |> Result.mapError (always "ERROR")


parseMultiplePagesApp : List AppPage -> String -> Result String App
parseMultiplePagesApp act sourceCode =
    Elm.Parser.parse sourceCode
        |> Result.map (emitMultiplePagesApp act)
        |> Result.mapError (always "ERROR")


emitSiglePageApp : RawFile.RawFile -> App
emitSiglePageApp file_ =
    let
        file =
            file_
                |> Processing.process Processing.init

        mod =
            { name =
                RawFile.moduleName file_
                    |> String.join "."
            , declarations = file.declarations |> List.map Node.value
            , interface = Interface.build file_
            }
    in
    findValidApp mod


emitMultiplePagesApp : List AppPage -> RawFile.RawFile -> App
emitMultiplePagesApp act file_ =
    let
        file =
            file_
                |> Processing.process Processing.init

        mod =
            { name =
                RawFile.moduleName file_
                    |> String.join "."
            , declarations = file.declarations |> List.map Node.value
            , interface = Interface.build file_
            }
    in
    findValidAppMultiple act mod


findValidApp : Module -> App
findValidApp mod =
    [ SinglePage.find mod
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault Empty


findValidAppMultiple : List AppPage -> Module -> App
findValidAppMultiple act mod =
    [ MultiplePages.find act mod
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault Empty
