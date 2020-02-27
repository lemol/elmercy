port module Main exposing (main)

import Elmercy.System.Main exposing (Model, Msg, init, subscriptions)
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

