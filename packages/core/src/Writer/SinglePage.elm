module Writer.SinglePage exposing (write)

import Data exposing (PageOptions, SubscriptionType(..))


write : PageOptions -> String
write { subscriptionType } =
    let
        x =
            case subscriptionType of
                Subscription0 ->
                    { browserFunction = "sandbox", subscriptionsImprot = "", subscriptionsField = "", viewFunction = "view", initFunction = "init" }

                Subscription2 ->
                    { browserFunction = "element", subscriptionsImprot = ", subscriptions", subscriptionsField = "\n        , subscriptions = subscriptions", viewFunction = "view", initFunction = "always init" }
    in
    template
        |> String.replace "{SUBSCRIPTIONS_IMPORT}" x.subscriptionsImprot
        |> String.replace "{SUBSCRIPTIONS_FIELD}" x.subscriptionsField
        |> String.replace "{BROWSER_FUNCTION}" x.browserFunction
        |> String.replace "{VIEW_FUNCTION}" x.viewFunction
        |> String.replace "{INIT_FUNCTION}" x.initFunction


template : String
template =
    """module App.Main exposing (main)

import Browser
import Main exposing (Model, Msg, init, update, view{SUBSCRIPTIONS_IMPORT})


main : Program () Model Msg
main =
    Browser.{BROWSER_FUNCTION}
        { init = {INIT_FUNCTION}
        , view = {VIEW_FUNCTION}
        , update = update{SUBSCRIPTIONS_FIELD}
        }
"""
