module Writer exposing (getResult)

import Data exposing (App(..))
import Writer.MultipleWriter as MultipleWriter
import Writer.SinglePage as SinglePage


getResult : App -> List ( String, String )
getResult app =
    case app of
        SinglePage x ->
            [ ( "Main.elm", SinglePage.write x ) ]

        MulitplePages pages ->
            MultipleWriter.write pages

        _ ->
            []
