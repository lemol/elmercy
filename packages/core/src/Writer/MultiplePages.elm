module Writer.MultiplePages exposing (write)

import Data exposing (AppConfig, AppPage, SubscriptionType(..))
import Elm.CodeGen exposing (..)
import Writer.MultiplePages.DataModule as DataModule
import Writer.MultiplePages.MainModule as MainModule
import Writer.MultiplePages.NotFoundModule as NotFoundModule
import Writer.MultiplePages.PageModule as PageModule
import Writer.MultiplePages.RoutesModule as RoutesModule
import Writer.MultiplePages.UtilsModule as UtilsModule


write : AppConfig -> List AppPage -> List ( String, String )
write config pages =
    [ ( "Data.elm", DataModule.write )
    , ( "Main.elm", MainModule.write config )
    , ( "NotFound.elm", NotFoundModule.write )
    , ( "Page.elm", PageModule.write config pages )
    , ( "Routes.elm", RoutesModule.write config pages )
    , ( "Utils.elm", UtilsModule.write )
    ]
