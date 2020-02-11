module ParseCounter exposing (..)

import Data exposing (..)
import Expect
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Counter! Single Page"
        [ test "with Msg -> Model -> Model" <|
            \_ ->
                let
                    result =
                        Parser.parseNextSource SinglePageAppType Empty mainFile

                    expected =
                        SinglePage
                            { moduleName = "Main"
                            , initType = Init1
                            , mainType = Main0
                            , updateType = Update3
                            , viewType = View2
                            , subscriptionType = Subscription0
                            }
                in
                result |> Expect.equal (Ok expected)
        ]


mainFile : String
mainFile =
    """
module Main exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- MESSAGES


type Msg
    = Increment
    | Decrement



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ button
            [ onClick Decrement ]
            [ text "-" ]
        , text (String.fromInt model)
        , button
            [ onClick Increment ]
            [ text "+" ]
        ]
"""
