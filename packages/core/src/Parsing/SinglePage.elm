module Parsing.SinglePage exposing (find)

import Data exposing (..)
import Elm.Interface as Interface
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Maybe.Extra


expectedInterface : List String
expectedInterface =
    [ "Model"
    , "Msg"
    , "init"
    , "update"
    , "view"
    ]


find : Module -> Maybe ExposedPage
find ({ interface } as mod_) =
    let
        checkInterface =
            expectedInterface
                |> List.any (\f -> Interface.exposesFunction f interface)

        mod =
            { declarations = filterDeclarations mod_
            , interface = interface
            }

        build =
            Just
                (\initType ->
                    \viewType ->
                        \updateType ->
                            { initType = initType
                            , viewType = viewType
                            , updateType = updateType
                            }
                )
                |> Maybe.Extra.andMap (checkInitType mod)
                |> Maybe.Extra.andMap (checkViewType mod)
                |> Maybe.Extra.andMap (checkUpdateType mod)
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


filterDeclarations : Module -> List Declaration
filterDeclarations { declarations } =
    let
        condition declaration =
            expectedInterface |> List.member (declarationName declaration)
    in
    declarations
        |> List.filter condition


declarationName : Declaration -> String
declarationName declaration =
    case declaration of
        Declaration.FunctionDeclaration x ->
            functionName x

        Declaration.AliasDeclaration x ->
            Node.value x.name

        Declaration.CustomTypeDeclaration x ->
            Node.value x.name

        _ ->
            "<not implemented>"


checkInitType : Module -> Maybe InitType
checkInitType { declarations } =
    let
        check declaration =
            case declaration of
                Declaration.FunctionDeclaration x ->
                    if functionName x == "init" then
                        getInitType (Maybe.map (Node.value >> .typeAnnotation >> Node.value) x.signature)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    declarations
        |> List.map check
        |> List.filterMap identity
        |> List.head


getInitType : Maybe TypeAnnotation.TypeAnnotation -> Maybe InitType
getInitType types =
    let
        get ta =
            case ta of
                TypeAnnotation.Typed (Node _ ( _, "Model" )) [] ->
                    Just Init1

                _ ->
                    Nothing
    in
    types
        |> Maybe.map get
        |> Maybe.Extra.join


checkViewType : Module -> Maybe ViewType
checkViewType { declarations } =
    let
        check declaration =
            case declaration of
                Declaration.FunctionDeclaration x ->
                    if functionName x == "view" then
                        getViewType (Maybe.map (Node.value >> .typeAnnotation >> Node.value) x.signature)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    declarations
        |> List.map check
        |> List.filterMap identity
        |> List.head


getViewType : Maybe TypeAnnotation.TypeAnnotation -> Maybe ViewType
getViewType types =
    let
        get ta =
            case ta of
                TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Html" )) _)) ->
                    Just View2

                _ ->
                    Nothing
    in
    types
        |> Maybe.map get
        |> Maybe.Extra.join


checkUpdateType : Module -> Maybe UpdateType
checkUpdateType { declarations } =
    let
        check declaration =
            case declaration of
                Declaration.FunctionDeclaration x ->
                    if functionName x == "update" then
                        getUpdateType (Maybe.map (Node.value >> .typeAnnotation >> Node.value) x.signature)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    declarations
        |> List.map check
        |> List.filterMap identity
        |> List.head


getUpdateType : Maybe TypeAnnotation.TypeAnnotation -> Maybe UpdateType
getUpdateType types =
    let
        get ta =
            case ta of
                TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Msg" )) [])) (Node _ (TypeAnnotation.FunctionTypeAnnotation (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])) (Node _ (TypeAnnotation.Typed (Node _ ( _, "Model" )) [])))) ->
                    Just Update3

                _ ->
                    Nothing
    in
    types
        |> Maybe.map get
        |> Maybe.Extra.join


functionName : Expression.Function -> String
functionName =
    .declaration >> Node.value >> .name >> Node.value


functionType : Expression.Function -> Maybe TypeAnnotation.TypeAnnotation
functionType f =
    f.signature
        |> Maybe.map Node.value
        |> Maybe.map .typeAnnotation
        |> Maybe.map Node.value


isHtmlReturnType : TypeAnnotation.TypeAnnotation -> Bool
isHtmlReturnType t =
    case t of
        TypeAnnotation.Typed (Node _ ( _, "Html" )) _ ->
            True

        TypeAnnotation.FunctionTypeAnnotation _ (Node _ x) ->
            isHtmlReturnType x

        _ ->
            False
