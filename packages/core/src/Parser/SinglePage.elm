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


find : Module -> Maybe App
find { interface, declarations } =
    let
        checkInterface =
            expectedInterface
                |> List.any (\f -> Interface.exposesFunction f interface)

        mod =
            { declarations = filterDeclarations expectedInterface declarations
            , interface = interface
            }

        build =
            Just
                (\initType viewType updateType ->
                    { initType = initType
                    , viewType = viewType
                    , updateType = updateType
                    }
                )
                |> Maybe.Extra.andMap (checkFunctionType "init" getInitType mod)
                |> Maybe.Extra.andMap (checkFunctionType "view" getViewType mod)
                |> Maybe.Extra.andMap (checkFunctionType "update" getUpdateType mod)
    in
    if checkInterface then
        build
            |> Maybe.map
                (\x ->
                    SinglePage
                        { initType = x.initType
                        , viewType = x.viewType
                        , updateType = x.updateType
                        , subscriptionType = Subscription0
                        }
                )

    else
        Nothing


getInitType : TypeAnnotation.TypeAnnotation -> Maybe InitType
getInitType ta =
    case ta of
        TypeAnnotation.Typed (Node _ ( _, "Model" )) [] ->
            Just Init1

        _ ->
            Nothing


getViewType : TypeAnnotation.TypeAnnotation -> Maybe ViewType
getViewType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Html" )) _)) ->
            Just View2

        _ ->
            Nothing


getUpdateType : TypeAnnotation.TypeAnnotation -> Maybe UpdateType
getUpdateType ta =
    case ta of
        TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])))) ->
            Just Update3

        _ ->
            Nothing
