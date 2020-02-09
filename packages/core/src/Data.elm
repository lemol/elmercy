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
    | SimpleHtml String
    | SinglePage PageOptions
    | MulitplePages (List AppPage)


type alias AppPage =
    { routeName : String
    , routePath : String
    , options : PageOptions
    }


type alias PageOptions =
    { initType : InitType
    , updateType : UpdateType
    , viewType : ViewType
    , subscriptionType : SubscriptionType
    }


type alias Module =
    { interface : Interface
    , declarations : List Declaration
    }


type InitType
    = Init1


type UpdateType
    = Update3


type ViewType
    = View2


type SubscriptionType
    = Subscription0
