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
            Just
                (\initType viewType updateType subscriptionType ->
                    { moduleName = name
                    , initType = initType
                    , viewType = viewType
                    , updateType = updateType
                    , subscriptionType = subscriptionType
                    }
                )
                |> Maybe.Extra.andMap (checkFunctionType "init" getInitType mod Nothing)
                |> Maybe.Extra.andMap (checkFunctionType "view" getViewType mod Nothing)
                |> Maybe.Extra.andMap (checkFunctionType "update" getUpdateType mod Nothing)
                |> Maybe.Extra.andMap (checkFunctionType "subscriptions" getSubscriptionType mod (Just Subscription0))
    in
    if checkInterface then
        build
            |> Maybe.map SinglePage

    else
        Nothing


getInitType : TypeAnnotation.TypeAnnotation -> Maybe InitType
getInitType ta =
    case ta of
        TypeAnnotation.Typed (Node _ ( _, "Model" )) [] ->
            Just Init1

        TypeAnnotation.Tupled [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) []), Node _ (TypeAnnotation.Typed (Node _ ( _, "Cmd" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) []) ]) ] ->
            Just Init2

        _ ->
            Nothing


getViewType : TypeAnnotation.TypeAnnotation -> Maybe ViewType
getViewType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Html" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) _) ])) ->
            Just View2

        _ ->
            Nothing


getUpdateType : TypeAnnotation.TypeAnnotation -> Maybe UpdateType
getUpdateType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])))) ->
            Just Update3

        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Tupled [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) []), Node _ (TypeAnnotation.Typed (Node _ ( _, "Cmd" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) []) ]) ])))) ->
            Just Update4

        _ ->
            Nothing


getSubscriptionType : TypeAnnotation.TypeAnnotation -> Maybe SubscriptionType
getSubscriptionType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Sub" )) [ Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) _) ])) ->
            Just Subscription2

        _ ->
            Just Subscription0
