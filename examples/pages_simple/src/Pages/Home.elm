module Pages.Home exposing (Model, Msg, update, view)

import App.Routes exposing (goTo, Route(..))
import Html exposing (Html, div, text)



-- MODEL


type alias Model =
    ()



-- MESSAGE


type Msg =
    GoTo Route



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoTo route ->
            ( model
            , goTo route
            )


-- VIEW


view : Html msg
view =
    div
        []
        [ text "Home page!"
        , button
            [ onClick (GoTo "/about") ]
            [ text "about" ]
        ]
