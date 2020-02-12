module Parser.OnePage exposing (find)

import Data exposing (..)
import Elm.Interface as Interface
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Parser.Utils exposing (..)


type alias FunctionGroup =
    { search : List String
    , required : List String
    }


simpleHtml : FunctionGroup
simpleHtml =
    { search = [ "view", "main" ]
    , required = [ "view", "main" ]
    }


singlePage : FunctionGroup
singlePage =
    { search =
        [ "Model"
        , "Msg"
        , "init"
        , "update"
        , "view"
        , "subscriptions"
        ]
    , required =
        [ "init"
        , "update"
        , "view"
        ]
    }


find : Module -> Maybe App
find ({ interface, declarations, name } as mod_) =
    let
        isSimpleHtml =
            simpleHtml.required
                |> List.any (\f -> Interface.exposesFunction f interface)

        isSinglePage =
            singlePage.required
                |> List.all (\f -> Interface.exposesFunction f interface)

        search =
            if isSinglePage then
                singlePage.search

            else if isSimpleHtml then
                simpleHtml.search

            else
                []

        mod =
            { mod_
                | declarations = filterDeclarations search declarations
            }

        result =
            OnePageApp
                { moduleName = name
                , initType = checkFunctionType "init" getInitType mod Init0
                , mainType = checkFunctionType "main" getMainType mod Main0
                , viewType = checkFunctionType "view" getViewType mod View0
                , updateType = checkFunctionType "update" getUpdateType mod Update0
                , subscriptionType = checkFunctionType "subscriptions" getSubscriptionType mod Subscription0
                }
    in
    if isSinglePage || isSimpleHtml then
        Just result

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
            InitUnknown


getMainType : TypeAnnotation.TypeAnnotation -> MainType
getMainType ta =
    case ta of
        TypeAnnotation.Typed (Node _ ( _, "Html" )) [ Node _ (TypeAnnotation.GenericType _) ] ->
            Main1

        _ ->
            MainUnknown


getViewType : TypeAnnotation.TypeAnnotation -> ViewType
getViewType ta =
    case ta of
        TypeAnnotation.Typed (Node _ ( _, "Html" )) [ Node _ (TypeAnnotation.GenericType _) ] ->
            View1

        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Html" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) _) ])) ->
            View2

        _ ->
            ViewUnknown


getUpdateType : TypeAnnotation.TypeAnnotation -> UpdateType
getUpdateType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])))) ->
            Update3

        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Tupled [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) []), Node _ (TypeAnnotation.Typed (Node _ ( _, "Cmd" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) []) ]) ])))) ->
            Update4

        _ ->
            UpdateUnknown


getSubscriptionType : TypeAnnotation.TypeAnnotation -> SubscriptionType
getSubscriptionType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Sub" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) _) ])) ->
            Subscription2

        _ ->
            SubscriptionUnknown
