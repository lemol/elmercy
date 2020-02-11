module Parser.MultiplePages exposing (find)

import Data exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Maybe.Extra
import Parser.SimpleHtml as SimpleHtml
import Parser.SinglePage as SinglePage
import Parser.Utils exposing (..)


find : List AppPage -> Module -> Maybe App
find act mod =
    let
        match =
            [ SinglePage.find mod
            , SimpleHtml.find mod
            ]
                |> List.filterMap identity
                |> List.map toMultiplePagesItem
                |> List.head
                |> Maybe.Extra.join
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        total =
            act ++ match
    in
    case total of
        [] ->
            Nothing

        x ->
            x
                |> MulitplePages
                |> Just


toMultiplePagesItem : App -> Maybe AppPage
toMultiplePagesItem app =
    case app of
        SimpleHtml moduleName _ ->
            Just
                { routeName = mkRouteName moduleName
                , routePath = mkPath moduleName
                , options =
                    { moduleName = moduleName
                    , initType = Init0
                    , updateType = Update0
                    , viewType = View1
                    , subscriptionType = Subscription0
                    }
                }

        SinglePage options ->
            Just
                { routeName = mkRouteName options.moduleName
                , routePath = mkPath options.moduleName
                , options = options
                }

        _ ->
            Nothing


mkPath : String -> String
mkPath moduleName =
    let
        inPages =
            moduleName |> String.startsWith "Pages"

        base =
            moduleName
                |> String.split "."
                |> List.drop
                    (if inPages then
                        1

                     else
                        0
                    )
                |> String.join "/"
                |> String.toLower
    in
    if base == "index" then
        "/"

    else
        "/" ++ base


mkRouteName : String -> String
mkRouteName moduleName =
    let
        inPages =
            moduleName |> String.startsWith "Pages"
    in
    moduleName
        |> String.split "."
        |> List.drop
            (if inPages then
                1

             else
                0
            )
        |> String.join ""
