module Writer.SinglePage exposing (write)

import Data exposing (PageOptions, SubscriptionType(..))


write : PageOptions -> String
write { subscriptionType } =
    let
        ( subscriptionsImprot, subscriptionsField ) =
            case subscriptionType of
                Subscription0 ->
                    ( "", "" )

                Subscription2 ->
                    ( ", subscriptions", "\n        , subscriptions = subscriptions" )
    in
    template
        |> String.replace "{SUBSCRIPTIONS_IMPORT}" subscriptionsImprot
        |> String.replace "{SUBSCRIPTIONS_FIELD}" subscriptionsField


template : String
template =
    """module App.Main exposing (main)

import Browser
import Main exposing (Model, Msg, init, update, view{SUBSCRIPTIONS_IMPORT})


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update{SUBSCRIPTIONS_FIELD}
        }
"""
