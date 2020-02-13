module Parser exposing (parseNextSource)

import Data exposing (App(..), AppType(..), Module)
import Elm.Interface as Interface
import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Node as Node
import Parser.MultiplePages as MultiplePages
import Parser.OnePage as OnePage


parseNextSource : AppType -> App -> String -> Result String App
parseNextSource appType app =
    case appType of
        OnePageAppType ->
            parse OnePage.find

        MulitplePagesAppType ->
            let
                list =
                    case app of
                        MulitplePagesApp _ x ->
                            x

                        _ ->
                            []
            in
            parse (MultiplePages.find list)

        UnknownAppType ->
            always <| Err "<unknown app type>"


parse : (Module -> Maybe App) -> String -> Result String App
parse finder sourceCode =
    Elm.Parser.parse sourceCode
        |> Result.map (emitApp finder)
        |> Result.mapError (always "<parse error>")


emitApp : (Module -> Maybe App) -> RawFile.RawFile -> App
emitApp finder file_ =
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
    mod
        |> finder
        |> Maybe.withDefault EmptyApp
