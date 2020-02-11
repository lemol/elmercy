module Parser.SinglePage exposing (find)

import Data exposing (..)
import Elm.Interface as Interface
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Maybe.Extra
import Parser.Utils exposing (..)


expectedInterface : List String
expectedInterface =
    [ "Model"
    , "Msg"
    , "init"
    , "update"
    , "view"
    ]


completeInterface : List String
completeInterface =
    "subscriptions" :: expectedInterface


find : Module -> Maybe App
find ({ interface, declarations, name } as mod_) =
    let
        checkInterface =
            expectedInterface
                |> List.any (\f -> Interface.exposesFunction f interface)

        mod =
            { mod_
                | declarations = filterDeclarations completeInterface declarations
            }

        build =
            { moduleName = name
            , initType = checkFunctionType "init" getInitType mod Init0
            , viewType = checkFunctionType "view" getViewType mod View0
            , updateType = checkFunctionType "update" getUpdateType mod Update0
            , subscriptionType = checkFunctionType "subscriptions" getSubscriptionType mod Subscription0
            }
    in
    if checkInterface then
        build |> Maybe.map SinglePage

    else
        Nothing


getInitType : TypeAnnotation.TypeAnnotation -> InitType
getInitType ta =
    case ta of
        TypeAnnotation.Typed (Node _ ( _, "Model" )) [] ->
            Init1

        TypeAnnotation.Tupled [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) []), Node _ (TypeAnnotation.Typed (Node _ ( _, "Cmd" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) []) ]) ] ->
            Init2

        _ ->
            Init0


getViewType : TypeAnnotation.TypeAnnotation -> ViewType
getViewType ta =
    case ta of
        TypeAnnotation.Typed (Node _ ( _, "Html" )) [ Node _ (TypeAnnotation.GenericType _) ] ->
            View1

        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Html" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) _) ])) ->
            View2

        _ ->
            View0


getUpdateType : TypeAnnotation.TypeAnnotation -> UpdateType
getUpdateType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])))) ->
            Update3

        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Tupled [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) []), Node _ (TypeAnnotation.Typed (Node _ ( _, "Cmd" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) []) ]) ])))) ->
            Update4

        _ ->
            Update0


getSubscriptionType : TypeAnnotation.TypeAnnotation -> SubscriptionType
getSubscriptionType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Sub" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) _) ])) ->
            Subscription2

        _ ->
            Subscription0
