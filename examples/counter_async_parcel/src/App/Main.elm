module App.Main exposing (main)

import Browser
import Main exposing (Model, Msg, init, update, view, subscriptions)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
