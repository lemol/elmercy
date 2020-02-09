module Data exposing (..)

import Elm.Interface exposing (Interface)
import Elm.Syntax.Declaration exposing (Declaration)


type ExposedPage
    = None
    | SimpleHtml String
    | SinglePage PageOptions


type alias AppPage =
    { routeName : String
    , routePath : String
    , exposedPage : ExposedPage
    }


type alias Module =
    { interface : Interface
    , declarations : List Declaration
    }


type alias PageOptions =
    { initType : InitType
    , updateType : UpdateType
    , viewType : ViewType
    , subscriptionType : SubscriptionType
    }


type InitType
    = Init1


type UpdateType
    = Update3


type ViewType
    = View2


type SubscriptionType
    = Subscription0
