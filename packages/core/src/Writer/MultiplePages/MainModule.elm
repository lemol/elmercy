module Writer.MultiplePages.MainModule exposing (write)

import Data exposing (AppConfig, SubscriptionType(..))
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty
import Utils exposing (..)


write : AppConfig -> String
write _ =
    let
        module_ =
            normalModule
                [ "App", "Main" ]
                [ funExpose "main" ]

        importList =
            [ importAppData
            , importAppPage
            , importAppRoutes
            , importAppUtils
            , importBrowser
            , importBrowserNavigation
            , importUrl
            , importUrlParser
            ]

        declarationList =
            [ mainDecl
            , flagsDecl
            , modelDecl
            , msgDecl
            , initDecl
            , updateDecl
            , subscriptionsDecl
            , viewDecl
            ]
    in
    CodeGen.file module_ importList declarationList Nothing
        |> Pretty.pretty 100



-- MAIN FUNCTION DECLARATION


mainAnn : TypeAnnotation
mainAnn =
    typed "Program"
        [ typed "Flags" []
        , typed "Model" []
        , typed "Msg" []
        ]


mainBody : Expression
mainBody =
    apply
        [ fqVal [ "Browser" ] "application"
        , record
            [ ( "init", val "init" )
            , ( "update", val "update" )
            , ( "view", val "view" )
            , ( "subscriptions", val "subscriptions" )
            , ( "onUrlRequest", construct "LinkClicked" [] )
            , ( "onUrlChange", construct "UrlChanged" [] )
            ]
        ]


mainDecl : Declaration
mainDecl =
    funDecl
        Nothing
        (Just mainAnn)
        "main"
        []
        mainBody



-- FLAGS TYPE


flagsDecl : Declaration
flagsDecl =
    aliasDecl
        Nothing
        "Flags"
        []
        unitAnn



-- MODEL TYPE


modelDecl : Declaration
modelDecl =
    aliasDecl
        Nothing
        "Model"
        []
        (recordAnn
            [ ( "app", typed "Data.Model" [] )
            , ( "page", typed "Page.Model" [] )
            ]
        )



-- MSG TYPE


msgDecl : Declaration
msgDecl =
    customTypeDecl
        Nothing
        "Msg"
        []
        [ ( "LinkClicked", [ typed "Browser.UrlRequest" [] ] )
        , ( "UrlChanged", [ typed "Url.Url" [] ] )
        , ( "PageMsg", [ typed "Page.Msg" [] ] )
        ]



-- INIT FUNCTION DECLARATION


initAnn : TypeAnnotation
initAnn =
    funAnn
        (typed "Flags" [])
        (funAnn
            (typed "Url.Url" [])
            (funAnn
                (typed "Navigation.Key" [])
                (tupleAnn
                    [ typed "Model" []
                    , typed "Cmd" [ typed "Msg" [] ]
                    ]
                )
            )
        )


initBody : Expression
initBody =
    let
        decls =
            [ letFunction "route"
                []
                (apply [ val "parseUrl", val "url" ])
            , letFunction "appModel"
                []
                (apply
                    [ fqVal [ "Data" ] "init"
                    , val "key"
                    , val "route"
                    ]
                )
            , letDestructuring
                (tuplePattern
                    [ namedPattern "pageModel" []
                    , namedPattern "pageCmd" []
                    ]
                )
                (apply
                    [ fqVal [ "Page" ] "init"
                    , val "route"
                    ]
                )
            , letFunction "model"
                []
                (record
                    [ ( "app", val "appModel" )
                    , ( "page", val "pageModel" )
                    ]
                )
            ]

        body =
            tuple
                [ val "model"
                , apply
                    [ fqVal [ "Cmd" ] "batch"
                    , list
                        [ apply
                            [ fqVal [ "Cmd" ] "map"
                            , construct "PageMsg" []
                            , val "pageCmd"
                            ]
                        ]
                    ]
                ]
    in
    letExpr decls body


initDecl : Declaration
initDecl =
    funDecl
        Nothing
        (Just initAnn)
        "init"
        [ allPattern, varPattern "url", varPattern "key" ]
        initBody



-- UPDATE FUNCTION


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


updateBody : Expression
updateBody =
    let
        linkClicked =
            caseExpr (val "urlRequest")
                [ ( fqNamedPattern [ "Browser" ] "Internal" [ varPattern "url" ]
                  , tuple
                        [ val "model"
                        , apply
                            [ fqVal [ "Navigation" ] "pushUrl"
                            , val "model"
                                |> flip access "app"
                                |> flip access "navigationKey"
                            , parens
                                (apply
                                    [ fqVal [ "Url" ] "toString"
                                    , val "url"
                                    ]
                                )
                            ]
                        ]
                  )
                , ( fqNamedPattern [ "Browser" ] "External" [ varPattern "href" ]
                  , tuple
                        [ val "model"
                        , apply
                            [ fqVal [ "Navigation" ] "load"
                            , val "href"
                            ]
                        ]
                  )
                ]
    in
    caseExpr
        (val "msg")
        [ ( namedPattern "LinkClicked" [ varPattern "urlRequest" ]
          , linkClicked
          )
        , ( namedPattern "UrlChanged" [ varPattern "url" ]
          , updateDeclUrlChanged
          )
        , ( namedPattern "PageMsg" [ varPattern "subMsg" ]
          , updateDeclPageMsg
          )
        ]


updateDeclUrlChanged : Expression
updateDeclUrlChanged =
    let
        decls =
            [ letFunction "app"
                []
                (val "model" |> flip access "app")
            , letFunction "route"
                []
                (apply [ val "parseUrl", val "url" ])
            , letFunction "newApp"
                []
                (update "app"
                    [ ( "route", val "route" )
                    ]
                )
            , letDestructuring
                (tuplePattern
                    [ namedPattern "newPage" []
                    , namedPattern "newPageCmd" []
                    ]
                )
                (apply
                    [ fqVal [ "Page" ] "enterRoute"
                    , val "model" |> flip access "page"
                    , val "route"
                    ]
                )
            ]
    in
    letExpr decls
        (tuple
            [ update "model"
                [ ( "page", val "newPage" )
                , ( "app", val "newApp" )
                ]
            , apply
                [ fqVal [ "Cmd" ] "batch"
                , list
                    [ apply
                        [ fqVal [ "Cmd" ] "map"
                        , construct "PageMsg" []
                        , val "newPageCmd"
                        ]
                    ]
                ]
            ]
        )


updateDeclPageMsg : Expression
updateDeclPageMsg =
    letExpr
        [ letDestructuring
            (tuplePattern
                [ namedPattern "newPage" []
                , namedPattern "newPageCmd" []
                ]
            )
            (apply
                [ fqVal [ "Page" ] "update"
                , val "subMsg"
                , val "model" |> flip access "page"
                ]
            )
        ]
        (tuple
            [ update "model"
                [ ( "page", val "newPage" )
                ]
            , apply
                [ fqVal [ "Cmd" ] "batch"
                , list
                    [ apply
                        [ fqVal [ "Cmd" ] "map"
                        , construct "PageMsg" []
                        , val "newPageCmd"
                        ]
                    ]
                ]
            ]
        )


updateDecl : Declaration
updateDecl =
    funDecl
        Nothing
        (Just updateAnn)
        "update"
        [ varPattern "msg", varPattern "model" ]
        updateBody



-- SUBSCRIPTIONS FUNCTION


subscriptionsAnn : TypeAnnotation
subscriptionsAnn =
    funAnn
        (typed "Model" [])
        (typed "Sub" [ typed "Msg" [] ])


subscriptionsBody : Expression
subscriptionsBody =
    apply
        [ fqVal [ "Sub" ] "batch"
        , list []
        ]


subscriptionsDecl : Declaration
subscriptionsDecl =
    funDecl
        Nothing
        (Just subscriptionsAnn)
        "subscriptions"
        [ allPattern ]
        subscriptionsBody



-- VIEW FUNCTION


viewAnn : TypeAnnotation
viewAnn =
    funAnn
        (typed "Model" [])
        (typed "Browser.Document" [ typed "Msg" [] ])


viewBody : Expression
viewBody =
    pipe
        (apply
            [ fqVal [ "Page" ] "view"
            , val "model"
                |> flip access "app"
                |> flip access "route"
            , val "model"
                |> flip access "page"
            ]
        )
        [ apply
            [ val "mapDocument"
            , construct "PageMsg" []
            ]
        ]


viewDecl : Declaration
viewDecl =
    funDecl
        Nothing
        (Just viewAnn)
        "view"
        [ varPattern "model" ]
        viewBody



-- IMPORTS


importAppData : Import
importAppData =
    importStmt
        [ "App", "Data" ]
        (Just [ "Data" ])
        Nothing


importAppPage : Import
importAppPage =
    importStmt
        [ "App", "Page" ]
        (Just [ "Page" ])
        Nothing


importAppRoutes : Import
importAppRoutes =
    importStmt
        [ "App", "Routes" ]
        Nothing
        (Just <| exposeExplicit [ funExpose "parseUrl" ])


importAppUtils : Import
importAppUtils =
    importStmt
        [ "App", "Utils" ]
        Nothing
        (Just <| exposeExplicit [ funExpose "mapDocument" ])


importBrowser : Import
importBrowser =
    importStmt
        [ "Browser" ]
        Nothing
        Nothing


importBrowserNavigation : Import
importBrowserNavigation =
    importStmt
        [ "Browser", "Navigation" ]
        (Just [ "Navigation" ])
        Nothing


importUrl : Import
importUrl =
    importStmt
        [ "Url" ]
        Nothing
        Nothing


importUrlParser : Import
importUrlParser =
    importStmt
        [ "Url", "Parser" ]
        Nothing
        (Just <| exposeExplicit [ funExpose "map" ])
