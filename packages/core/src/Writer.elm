module Writer exposing (getResult)

import Data exposing (App(..))
import Writer.SimpleHtml as SimpleHtml
import Writer.SinglePage as SinglePage


getResult : App -> List ( String, String )
getResult app =
    case app of
        SimpleHtml moduleName functionName ->
            [ ( "Main.elm", SimpleHtml.write moduleName functionName ) ]

        SinglePage x ->
            [ ( "Main.elm", SinglePage.write x ) ]

        MulitplePages pages ->
            [ ( "Data.elm", Debug.todo "" )
            , ( "Main.elm", Debug.todo "" )
            , ( "Page.elm", Debug.todo "" )
            , ( "Routes.elm", Debug.todo "" )
            , ( "Utils.elm", Debug.todo "" )
            ]

        _ ->
            []
