module Writer exposing (createFiles)


createFiles : List String -> List ( String, String )
createFiles plugins =
    [ ( "elm.json", elmJsonFile plugins )
    , ( "src/index.js", indexJsFile )
    , ( "src/Main.elm", mainElmFile )
    , ( "src/Plugins.elm", pluginsElmFile plugins )
    ]


elmJsonFile : List String -> String
elmJsonFile plugins =
    let
        directDependencies =
            plugins
                |> List.map (\x -> ",\"" ++ x ++ "\":" ++ "\"" ++ "latest" ++ "\"")
                |> String.join ""
    in
    """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"{{DIRECT_DEPENDENCIES}}
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}

    """
        |> String.replace "{{DIRECT_DEPENDENCIES}}" directDependencies


indexJsFile : String
indexJsFile =
    """import { Elm } from "./Main";

export function initApp({ projectPath, watch, outputDir }) {
  return Elm.Main.init({ flags: null });
}
    """


mainElmFile : String
mainElmFile =
    """port module Main exposing (main)

import Elmercy.System.Main exposing (Model, Msg)
import Plugins



-- OPTIONS

options : Elmercy.System.Main.Options
options =
    { requestSourceCode = requestSourceCode
    , writeResult = writeResult
    , readSourcePaths = readSourcePaths
    , readSourceCode = readSourceCode
    , plugins = Plugins.plugins
    }



-- PROGRAM


main : Program () Model Msg
main =
    Elmercy.System.Main.main options



-- PORTS


port requestSourceCode : String -> Cmd msg


port writeResult : List ( String, String ) -> Cmd msg


port readSourcePaths : (List String -> msg) -> Sub msg


port readSourceCode : (( String, String ) -> msg) -> Sub msg

    """


pluginsElmFile : List String -> String
pluginsElmFile plugins =
    let
        importPlugins =
            plugins
                |> List.map (\x -> "import " ++ x)
                |> String.join "\n"

        listPlugins =
            plugins
                |> List.map (\x -> x ++ ".plugin")
                |> String.join ","
    in
    """module Plugins exposing (plugins)

import Elmercy.System exposing (Plugin)
{IMPORT_PLUGINS}


plugins : List Plugin
plugins =
  [ {LIST_PLUGINS}
  ]
    """
        |> String.replace "{IMPORT_PLUGINS}" importPlugins
        |> String.replace "{LIST_PLUGINS}" listPlugins
