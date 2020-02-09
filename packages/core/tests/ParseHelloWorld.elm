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
                        Parser.parseNextSource SimpleHtmlAppType Empty mainFileExposeView

                    expected =
                        SimpleHtml "view"
                in
                result |> Expect.equal (Ok expected)
        , test "hello world exposing main : Html msg" <|
            \_ ->
                let
                    result =
                        Parser.parseNextSource SimpleHtmlAppType Empty mainFileExposeMain

                    expected =
                        SimpleHtml "main"
                in
                result |> Expect.equal (Ok expected)
        ]
