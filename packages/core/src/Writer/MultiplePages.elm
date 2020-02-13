module Writer.MultiplePages exposing (write)

import Data exposing (AppConfig, AppPage, SubscriptionType(..))
import Elm.CodeGen exposing (..)
import Writer.MultiplePages.DataModule as DataModule
import Writer.MultiplePages.RoutesModule as RoutesModule


write : AppConfig -> List AppPage -> List ( String, String )
write config pages =
    [ ( "Data.elm", DataModule.write )
    , ( "Main.elm", "-- Main.elm" )
    , ( "Page.elm", "-- Page.elm" )
    , ( "Routes.elm", RoutesModule.write config pages )
    , ( "Utils.elm", "-- Utils.elm" )
    ]
