module ParseHelloWorld exposing (..)

import Data exposing (..)
import Expect
import Parser
import Test exposing (..)


mainFileExposeView : String
mainFileExposeView =
    """module Main exposing (view)

import Html exposing (Html, text)

view : Html msg
view =
    Html.text "Hello world!"
"""


mainFileExposeMain : String
mainFileExposeMain =
    """module Main exposing (main)

import Html exposing (Html, text)

main : Html msg
main =
    Html.text "Hello world!"
"""


suite : Test
suite =
    describe "Hello world! Simple case"
        [ test "hello world exposing view : Html msg" <|
            \_ ->
                let
                    result =
                        Parser.parseNextSource SinglePageAppType Empty mainFileExposeView

                    expected =
                        SinglePage
                            { moduleName = "Main"
                            , initType = Init0
                            , mainType = Main0
                            , updateType = Update0
                            , viewType = View1
                            , subscriptionType = Subscription0
                            }
                in
                result |> Expect.equal (Ok expected)
        , test "hello world exposing main : Html msg" <|
            \_ ->
                let
                    result =
                        Parser.parseNextSource SinglePageAppType Empty mainFileExposeMain

                    expected =
                        SinglePage
                            { moduleName = "Main"
                            , initType = Init0
                            , mainType = Main1
                            , updateType = Update0
                            , viewType = View0
                            , subscriptionType = Subscription0
                            }
                in
                result |> Expect.equal (Ok expected)
        ]
