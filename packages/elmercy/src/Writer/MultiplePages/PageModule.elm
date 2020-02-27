module Writer.MultiplePages.PageModule exposing (write)

import Data exposing (..)
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty
import Utils exposing (..)


write : AppConfig -> List AppPage -> String
write config pages_ =
    let
        allPages =
            pages_ ++ [ config.notFound ]

        module_ =
            normalModule
                [ "App", "Page" ]
                [ typeOrAliasExpose "Model"
                , openTypeExpose "Msg"
                , funExpose "enterRoute"
                , funExpose "init"
                , funExpose "subscriptions"
                , funExpose "update"
                , funExpose "view"
                ]

        importList =
            [ importAppRoutes
            , importAppUtils
            , importBrowser
            , importHtml
            ]
                ++ importPages allPages

        declarationList =
            [ modelDecl allPages
            , initDecl allPages
            , enterRouteDecl allPages
            , msgDecl allPages
            , updateDecl allPages
            , viewDecl allPages
            , viewEmptyDecl
            , toDocumentDecl
            , subscriptionsDecl allPages
            ]
    in
    CodeGen.file module_ importList declarationList Nothing
        |> Pretty.pretty 100



-- MODEL TYPE ALIAS


modelDecl : List AppPage -> Declaration
modelDecl pages =
    let
        pageRecordField page =
            ( camelCasePage page.routeName
            , typed "Maybe"
                [ typed (page.options.moduleName ++ ".Model") [] ]
            )
    in
    aliasDecl
        Nothing
        "Model"
        []
        (pages
            |> filterPageWithModel
            |> List.map pageRecordField
            |> recordAnn
        )



-- INIT FUNCTION DECLARATION


initAnn : TypeAnnotation
initAnn =
    funAnn
        (typed "Routes.Route" [])
        (tupleAnn
            [ typed "Model" []
            , typed "Cmd" [ typed "Msg" [] ]
            ]
        )


initBody : List AppPage -> Expression
initBody pages =
    let
        decls =
            [ letFunction "model"
                []
                (pages
                    |> filterPageWithModel
                    |> List.map
                        (\page ->
                            ( camelCasePage page.routeName
                            , construct "Nothing" []
                            )
                        )
                    |> record
                )
            ]

        body =
            apply
                [ val "enterRoute"
                , val "model"
                , val "route"
                ]
    in
    letExpr decls body


initDecl : List AppPage -> Declaration
initDecl pages =
    funDecl
        Nothing
        (Just initAnn)
        "init"
        [ varPattern "route" ]
        (initBody pages)



-- ENTERROUTE FUNCTION


enterRouteAnn : TypeAnnotation
enterRouteAnn =
    funAnn
        (typed "Model" [])
        (funAnn
            (typed "Routes.Route" [])
            (tupleAnn
                [ typed "Model" []
                , typed "Cmd" [ typed "Msg" [] ]
                ]
            )
        )


enterRouteBody : List AppPage -> Expression
enterRouteBody pages =
    let
        routeCase page =
            ( routePattern page
            , if pageHasCmd page then
                letExpr
                    [ letDestructuring
                        (tuplePattern
                            [ varPattern "pageModel", varPattern "pageCmd" ]
                        )
                      <|
                        val (page.options.moduleName ++ ".init")
                    ]
                    (tuple
                        [ update "model"
                            [ ( camelCasePage page.routeName
                              , pipe
                                    (val "model" |> flip access (camelCasePage page.routeName))
                                    [ apply
                                        [ fqVal [ "Maybe" ] "withDefault"
                                        , val "pageModel"
                                        ]
                                    , val "Just"
                                    ]
                              )
                            ]
                        , apply
                            [ fqVal [ "Cmd" ] "map"
                            , msgConstructor page
                            , val "pageCmd"
                            ]
                        ]
                    )

              else
                letExpr
                    [ letVal "pageModel" <|
                        val (page.options.moduleName ++ ".init")
                    ]
                    (tuple
                        [ update "model"
                            [ ( camelCasePage page.routeName
                              , pipe
                                    (val "model" |> flip access (camelCasePage page.routeName))
                                    [ apply
                                        [ fqVal [ "Maybe" ] "withDefault"
                                        , val "pageModel"
                                        ]
                                    , val "Just"
                                    ]
                              )
                            ]
                        , fqVal [ "Cmd" ] "none"
                        ]
                    )
            )

        elseCase =
            ( allPattern
            , tuple
                [ val "model"
                , fqVal [ "Cmd" ] "none"
                ]
            )
    in
    caseExpr
        (val "route")
        ((pages
            |> filterPageWithModel
            |> List.map routeCase
         )
            ++ [ elseCase ]
        )


enterRouteDecl : List AppPage -> Declaration
enterRouteDecl pages =
    funDecl
        Nothing
        (Just enterRouteAnn)
        "enterRoute"
        [ varPattern "model", varPattern "route" ]
        (enterRouteBody pages)



--  MSG  DECL


msgDecl : List AppPage -> Declaration
msgDecl pages_ =
    let
        pages =
            pages_ |> filterPageWithModel

        pageMsg page =
            ( pageName page.routeName ++ "Msg"
            , [ typed (page.options.moduleName ++ ".Msg") [] ]
            )

        defaultMsg =
            [ ( "NoMsg"
              , []
              )
            ]
    in
    customTypeDecl
        Nothing
        "Msg"
        []
        (if List.isEmpty pages then
            defaultMsg

         else
            pages
                |> List.map pageMsg
        )



-- UPDATE DECL


updateAnn : TypeAnnotation
updateAnn =
    funAnn
        (typed "Msg" [])
        (funAnn
            (typed "Model" [])
            (tupleAnn
                [ typed "Model" []
                , typed "Cmd" [ typed "Msg" [] ]
                ]
            )
        )


updateBody : List AppPage -> Expression
updateBody pages_ =
    let
        pages =
            pages_ |> filterPageWithModel

        updatePageCase page =
            ( updatePagePattern page
            , if pageHasCmd page then
                letExpr
                    [ letDestructuring
                        (tuplePattern
                            [ varPattern "pageModel"
                            , varPattern "pageCmd"
                            ]
                        )
                      <|
                        apply
                            [ val "updateHelper2"
                            , val (page.options.moduleName ++ ".update")
                            , val "subMsg"
                            , val "model" |> flip access (camelCasePage page.routeName)
                            ]
                    ]
                    (tuple
                        [ update "model"
                            [ ( camelCasePage page.routeName
                              , val "pageModel"
                              )
                            ]
                        , apply
                            [ fqVal [ "Cmd" ] "map"
                            , msgConstructor page
                            , val "pageCmd"
                            ]
                        ]
                    )

              else
                letExpr
                    [ letVal "pageModel" <|
                        apply
                            [ val "updateHelper1"
                            , val (page.options.moduleName ++ ".update")
                            , val "subMsg"
                            , val "model" |> flip access (camelCasePage page.routeName)
                            ]
                    ]
                    (tuple
                        [ update "model"
                            [ ( camelCasePage page.routeName
                              , val "pageModel"
                              )
                            ]
                        , fqVal [ "Cmd" ] "none"
                        ]
                    )
            )
    in
    if List.isEmpty pages then
        tuple
            [ val "model"
            , fqVal [ "Cmd" ] "none"
            ]

    else
        caseExpr
            (val "msg")
            (pages
                |> List.map updatePageCase
            )


updateDecl : List AppPage -> Declaration
updateDecl pages =
    funDecl
        Nothing
        (Just updateAnn)
        "update"
        [ varPattern "msg", varPattern "model" ]
        (updateBody pages)



-- VIEW FUNCTION


viewAnn : TypeAnnotation
viewAnn =
    typed "Routes.Route" []
        |> flip funAnn
            (typed "Model" []
                |> flip funAnn (typed "Browser.Document" [ typed "Msg" [] ])
            )


viewBody : List AppPage -> Expression
viewBody pages =
    let
        viewPageCase page =
            ( routePattern page
            , viewPage page
            )

        viewPage page =
            if pageHasModel page then
                pipe
                    (val "model" |> flip access (camelCasePage page.routeName))
                    [ apply
                        [ fqVal [ "Maybe" ] "map"
                        , fqVal (toModuleName page.options.moduleName) "view"
                        ]
                    , apply
                        [ fqVal [ "Maybe" ] "map"
                        , parens <|
                            apply
                                [ val "toDocument"
                                , string ""
                                ]
                        ]
                    , apply
                        [ fqVal [ "Maybe" ] "withDefault"
                        , val "viewEmpty"
                        ]
                    , apply
                        [ val "mapDocument"
                        , msgConstructor page
                        ]
                    ]

            else
                pipe
                    (fqVal (toModuleName page.options.moduleName) "view")
                    [ apply
                        [ val "toDocument"
                        , string ""
                        ]
                    ]
    in
    caseExpr
        (val "route")
        (pages
            |> List.map viewPageCase
        )


viewDecl : List AppPage -> Declaration
viewDecl pages =
    funDecl
        Nothing
        (Just viewAnn)
        "view"
        [ varPattern "route", varPattern "model" ]
        (viewBody pages)



-- INIT FUNCTION DECLARATION


subscriptionsAnn : TypeAnnotation
subscriptionsAnn =
    funAnn
        (typed "Model" [])
        (typed "Sub" [ typed "Msg" [] ])


subscriptionsBody : List AppPage -> Expression
subscriptionsBody pages_ =
    let
        pages =
            pages_
                |> List.filter pageHasSubscriptions

        pageSubscription page =
            case page.options.subscriptionType of
                Subscription1 ->
                    apply
                        [ fqVal [ "Sub" ] "map"
                        , msgConstructor page
                        , val (page.options.moduleName ++ ".subscriptions")
                        ]

                Subscription2 ->
                    pipe
                        (val "model" |> flip access "counterAsync")
                        [ apply
                            [ fqVal [ "Maybe" ] "map"
                            , val (page.options.moduleName ++ ".subscriptions")
                            ]
                        , apply
                            [ fqVal [ "Maybe" ] "map"
                            , parens
                                (apply
                                    [ fqVal [ "Sub" ] "map"
                                    , msgConstructor page
                                    ]
                                )
                            ]
                        ]

                _ ->
                    string "<invalid subsription type>"
    in
    pipe
        (list (pages |> List.map pageSubscription))
        [ apply
            [ fqVal [ "List" ] "filterMap"
            , val "identity"
            ]
        , fqVal [ "Sub" ] "batch"
        ]


subscriptionsDecl : List AppPage -> Declaration
subscriptionsDecl pages =
    funDecl
        Nothing
        (Just subscriptionsAnn)
        "subscriptions"
        [ varPattern "model" ]
        (subscriptionsBody pages)



-- VIEWEMPTY DECL


viewEmptyAnn : TypeAnnotation
viewEmptyAnn =
    typed "Browser.Document" [ typeVar "msg" ]


viewEmptyBody : Expression
viewEmptyBody =
    record
        [ ( "title"
          , string ""
          )
        , ( "body"
          , list
                [ apply
                    [ val "text", string "Page not loaded" ]
                ]
          )
        ]


viewEmptyDecl : Declaration
viewEmptyDecl =
    funDecl
        Nothing
        (Just viewEmptyAnn)
        "viewEmpty"
        []
        viewEmptyBody



-- TO_DOCUMENT DECL


toDocumentAnn : TypeAnnotation
toDocumentAnn =
    typed "String" []
        |> flip funAnn
            (typed "Html" [ typeVar "msg" ]
                |> flip funAnn (typed "Browser.Document" [ typeVar "msg" ])
            )


toDocumentBody : Expression
toDocumentBody =
    record
        [ ( "title"
          , val "title"
          )
        , ( "body"
          , list
                [ val "body"
                ]
          )
        ]


toDocumentDecl : Declaration
toDocumentDecl =
    funDecl
        Nothing
        (Just toDocumentAnn)
        "toDocument"
        [ varPattern "title", varPattern "body" ]
        toDocumentBody



-- IMPORTS


importAppRoutes : Import
importAppRoutes =
    importStmt
        [ "App", "Routes" ]
        (Just [ "Routes" ])
        Nothing


importAppUtils : Import
importAppUtils =
    importStmt
        [ "App", "Utils" ]
        Nothing
        (Just <|
            exposeExplicit
                [ funExpose "mapDocument"
                , funExpose "updateHelper1"
                , funExpose "updateHelper2"
                ]
        )


importBrowser : Import
importBrowser =
    importStmt
        [ "Browser" ]
        Nothing
        Nothing


importHtml : Import
importHtml =
    importStmt
        [ "Html" ]
        Nothing
        (Just <| exposeExplicit [ closedTypeExpose "Html", funExpose "text" ])


importPages : List AppPage -> List Import
importPages pages =
    let
        importPage page =
            importStmt
                (toModuleName page.options.moduleName)
                Nothing
                Nothing
    in
    pages |> List.map importPage



-- UTILS


toModuleName : String -> ModuleName
toModuleName =
    String.split "."


pageHasCmd : AppPage -> Bool
pageHasCmd page =
    pageHasModel page
        && page.options.initType
        /= Init1


pageHasSubscriptions : AppPage -> Bool
pageHasSubscriptions page =
    page.options.subscriptionType
        /= SubscriptionUnknown
        && page.options.subscriptionType
        /= Subscription0


pageHasModel : AppPage -> Bool
pageHasModel page =
    page.options.initType
        /= InitUnknown
        && page.options.initType
        /= Init0


filterPageWithModel : List AppPage -> List AppPage
filterPageWithModel =
    List.filter pageHasModel


pageName : String -> String
pageName routeName =
    let
        sufix =
            if
                String.startsWith "Pages." routeName
                    || String.startsWith "App." routeName
            then
                routeName
                    |> String.split "."
                    |> List.drop 1
                    |> String.join ""

            else
                routeName
    in
    sufix


camelCasePage : String -> String
camelCasePage routeName =
    let
        sufix =
            pageName routeName
    in
    (sufix |> String.left 1 |> String.toLower) ++ (sufix |> String.dropLeft 1)


routePattern : AppPage -> Pattern
routePattern page =
    fqNamedPattern [ "Routes" ] page.routeName []


updatePagePattern : AppPage -> Pattern
updatePagePattern page =
    namedPattern (pageName page.routeName ++ "Msg") [ varPattern "subMsg" ]


msgConstructor : AppPage -> Expression
msgConstructor page =
    val (pageName page.routeName ++ "Msg")
