module Writer exposing (getResult)

import Data exposing (App(..))
import Writer.SimpleHtml as SimpleHtml
import Writer.SinglePage as SinglePage


getResult : App -> List ( String, String )
getResult app =
    case app of
        SimpleHtml x ->
            [ ( "App/Main.elm", SimpleHtml.write x ) ]

        SinglePage x ->
            [ ( "App/Main.elm", SinglePage.write x ) ]

        _ ->
            []
