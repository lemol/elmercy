module Data exposing (..)

import Elm.Interface exposing (Interface)
import Elm.Syntax.Declaration exposing (Declaration)


type AppType
    = UnknownAppType
    | OnePageAppType
    | MulitplePagesAppType


type App
    = EmptyApp
    | OnePageApp PageOptions
    | MulitplePagesApp AppConfig (List AppPage)


type alias AppPage =
    { routeName : String
    , routePath : String
    , options : PageOptions
    }


type alias PageOptions =
    { moduleName : String
    , initType : InitType
    , mainType : MainType
    , updateType : UpdateType
    , viewType : ViewType
    , subscriptionType : SubscriptionType
    }


type alias AppConfig =
    { notFound : AppPage }


type alias Module =
    { name : String
    , interface : Interface
    , declarations : List Declaration
    }


type MainType
    = MainUnknown
    | Main0 -- no main
    | Main1 -- main : Html msg


type InitType
    = InitUnknown
    | Init0 -- no init
    | Init1 -- init : Model
    | Init2 -- init : (Model, Cmd Msg)


type UpdateType
    = UpdateUnknown
    | Update0 -- no update
    | Update3 -- update : Msg -> Model -> Model
    | Update4 -- update : Msg -> Model -> (Model, Cmd Msg)


type ViewType
    = ViewUnknown
    | View0 -- no view
    | View1 -- view : Html msg
    | View2 -- view : Model -> Html Msg


type SubscriptionType
    = SubscriptionUnknown
    | Subscription0 -- no subscriptions function
    | Subscription1 -- subscriptions : Sub Msg
    | Subscription2 -- subscriptions : Model -> Sub Msg


defaultAppConfig : AppConfig
defaultAppConfig =
    { notFound =
        { routeName = "NotFound"
        , routePath = "/404"
        , options =
            { moduleName = "App.NotFound"
            , initType = Init0
            , mainType = Main0
            , updateType = Update0
            , viewType = View1
            , subscriptionType = Subscription0
            }
        }
    }
