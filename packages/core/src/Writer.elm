module Writer exposing (getResult)

import Data exposing (App(..))
import Writer.SinglePage as SinglePage


getResult : App -> List ( String, String )
getResult app =
    case app of
        SinglePage x ->
            [ ( "Main.elm", SinglePage.write x ) ]

        MulitplePages pages ->
            [ ( "Data.elm", "-- Data.elm" )
            , ( "Main.elm", "-- Main.elm" )
            , ( "Page.elm", "-- Page.elm" )
            , ( "Routes.elm", "-- Routes.elm" )
            , ( "Utils.elm", "-- Utils.elm" )
            ]

        _ ->
            []
