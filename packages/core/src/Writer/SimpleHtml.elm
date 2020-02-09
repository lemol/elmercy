module Writer.SimpleHtml exposing (write)


write : String -> String
write functionName =
    """module App.Main exposing (main)

import Html exposing (Html)
import Main


main : Html msg
main =
    Main.{FUNCTION_NAME}
"""
        |> String.replace "{FUNCTION_NAME}" functionName
