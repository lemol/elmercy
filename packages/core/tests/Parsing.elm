module Parsing exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


elmJsonFile =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.4",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""


mainFile =
    """module Main exposing (view)

import Html exposing (Html, text)

view =
    Html.text "Hello world!"

num =
    "Asd"
"""


project =
    [ { filePath = "elm.json"
      , sourceCode = elmJsonFile
      }
    , { filePath = "src/Main.elm"
      , sourceCode = mainFile
      }
    ]


suite : Test
suite =
    describe "Main module"
        [ test "hello world" <|
            \_ ->
                let
                    result =
                        Main.parseProject project
                            |> Result.map List.head
                            |> Result.map
                                (Maybe.withDefault
                                    { routeName = "NOPE"
                                    , routePath = "NOPE"
                                    , exposedPage = Main.SimpleHtml "NOPE"
                                    }
                                )

                    expected =
                        { routeName = "Main"
                        , routePath = "/"
                        , exposedPage = Main.SimpleHtml "view"
                        }
                in
                result |> Expect.equal (Ok expected)
        ]
