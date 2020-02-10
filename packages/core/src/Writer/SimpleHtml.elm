module Writer.SimpleHtml exposing (write)


write : String -> String -> String
write moduleName functionName =
    """module App.Main exposing (main)

import Html exposing (Html)
import {MODULE_NAME}


main : Html msg
main =
    Main.{FUNCTION_NAME}
"""
        |> String.replace "{MODULE_NAME}" moduleName
        |> String.replace "{FUNCTION_NAME}" functionName
