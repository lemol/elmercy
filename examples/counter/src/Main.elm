module Main exposing (Model, Msg, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { counter : Int
    }



-- MESSAGES


type Msg
    = Increment
    | Decrement



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = counter + 1 }

        Decrement ->
            { model | counter = counter - 1 }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ button
            [ onClick Decrement ]
            [ text "-" ]
        , text (String.fromInt model.counter)
        , button
            [ onClick Increment ]
            [ text "+" ]
        ]
