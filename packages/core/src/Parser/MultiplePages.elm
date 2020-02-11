module Parser.MultiplePages exposing (find)

import Data exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Maybe.Extra
import Parser.SinglePage as SinglePage
import Parser.Utils exposing (..)


find : List AppPage -> Module -> Maybe App
find act mod =
    let
        match =
            [ SinglePage.find mod
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
