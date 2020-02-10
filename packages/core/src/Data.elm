module Data exposing (..)

import Elm.Interface exposing (Interface)
import Elm.Syntax.Declaration exposing (Declaration)


type AppType
    = Unknown
    | SimpleHtmlAppType
    | SinglePageAppType
    | MulitplePagesAppType


type App
    = Empty
    | SimpleHtml String String
    | SinglePage PageOptions
    | MulitplePages (List AppPage)


type alias AppPage =
    { routeName : String
    , routePath : String
    , options : PageOptions
    }


type alias PageOptions =
    { moduleName : String
    , initType : InitType
    , updateType : UpdateType
    , viewType : ViewType
    , subscriptionType : SubscriptionType
    }


type alias Module =
    { name : String
    , interface : Interface
    , declarations : List Declaration
    }


type InitType
    = Init1 -- init : Model
    | Init2 -- init : (Model, Cmd Msg)


type UpdateType
    = Update3 -- update : Msg -> Model -> Model
    | Update4 -- update : Msg -> Model -> (Model, Cmd Msg)


type ViewType
    = View2 -- view : Model -> Html Msg


type SubscriptionType
    = Subscription0 -- no subscriptions function
    | Subscription2 -- subscriptions : Model -> Sub Msg
