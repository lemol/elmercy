module ParsingHelloWorld exposing (..)

import Data exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


mainFileExposeView =
    """module Main exposing (view)

import Html exposing (Html, text)

view : Html msg
view =
    Html.text "Hello world!"
"""


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
                        Main.parsePage mainFileExposeView

                    expected =
                        { routeName = "Main"
                        , routePath = "/"
                        , exposedPage = SimpleHtml "view"
                        }
                in
                result |> Expect.equal (Ok expected)
        , test "hello world exposing main : Html msg" <|
            \_ ->
                let
                    result =
                        Main.parsePage mainFileExposeMain

                    expected =
                        { routeName = "Main"
                        , routePath = "/"
                        , exposedPage = SimpleHtml "main"
                        }
                in
                result |> Expect.equal (Ok expected)
        ]
