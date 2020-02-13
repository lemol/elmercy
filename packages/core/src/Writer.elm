module Writer exposing (result)

import Data exposing (App(..))
import Writer.MultiplePages as MultiplePages
import Writer.OnePage as OnePage


result : App -> List ( String, String )
result app =
    case app of
        OnePageApp options ->
            OnePage.write options

        MulitplePagesApp config pages ->
            MultiplePages.write config pages

        _ ->
            []
