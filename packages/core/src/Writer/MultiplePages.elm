module Writer.MultiplePages exposing (write)

import Data exposing (AppPage, SubscriptionType(..))
import Elm.CodeGen exposing (..)
import Writer.MultiplePages.DataModule as DataModule
import Writer.MultiplePages.RoutesModule as RoutesModule


write : List AppPage -> List ( String, String )
write pages =
    [ ( "Data.elm", DataModule.write )
    , ( "Main.elm", "-- Main.elm" )
    , ( "Page.elm", "-- Page.elm" )
    , ( "Routes.elm", RoutesModule.write pages )
    , ( "Utils.elm", "-- Utils.elm" )
    ]
