module Parser exposing (parseNextSource)

import Data exposing (App(..), AppType(..), Module)
import Elm.Interface as Interface
import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Node as Node
import Parser.SimpleHtml as SimpleHtml
import Parser.SinglePage as SinglePage


parseNextSource : AppType -> App -> String -> Result String App
parseNextSource appType _ source =
    case appType of
        SimpleHtmlAppType ->
            parseSinglePageApp source

        SinglePageAppType ->
            parseSinglePageApp source

        _ ->
            Err "<not implemented yet>"


parseSinglePageApp : String -> Result String App
parseSinglePageApp sourceCode =
    Elm.Parser.parse sourceCode
        |> Result.map emitSiglePageApp
        |> Result.mapError (always "ERROR")


emitSiglePageApp : RawFile.RawFile -> App
emitSiglePageApp file_ =
    let
        file =
            file_
                |> Processing.process Processing.init

        mod =
            { declarations = file.declarations |> List.map Node.value
            , interface = Interface.build file_
            }
    in
    findValidApp mod


findValidApp : Module -> App
findValidApp mod =
    [ SinglePage.find mod
    , SimpleHtml.find mod
    ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault Empty
