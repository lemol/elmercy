module Writer.MultiplePages.MainModule exposing (write)

import Data exposing (AppConfig, AppPage, SubscriptionType(..))
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty


write : AppConfig -> String
write config =
    let
        module_ =
            normalModule
                [ "App", "Main" ]
                [ funExpose "main" ]

        importList =
            [ importAppData
            , importAppPage
            , importAppRoutes
            , importBrowser
            , importBrowserNavigation
            , importUrl
            , importUrlParser
            ]

        declarationList =
            [ mainDecl
            , flagsDecl
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
        |> Just


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
        mainAnn
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
        [ ( "LinkClicked", typed "Browser.UrlRequest" [] )
        , ( "UrlChanged", typed "Url.Url" [] )
        , ( "PageMsg", typed "Page.Msg" [] )
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
                    [ fqVal [ "App" ] "init"
                    , val "key"
                    , val "route"
                    ]
                )
            , letDestructuring
                (tuplePattern
                    [ namedPattern "pageModel"
                    , namedPattern "pageCmd"
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
                (val "model")
                (apply
                    [ fqVal [ "Cmd" ] "batch"
                    , list
                        [ apply
                            [ fqVal [ "Cmd" ] "map"
                            , construct "PageMsg" []
                            , val "pageCmd"
                            ]
                        ]
                    ]
                )
    in
    letExpr decls body


initDecl : Declaration
initDecl =
    funDecl
        Nothing
        initAnn
        "init"
        []
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
    in
    caseExpr
        (val "msg")
        []

updateDecl : Declaration
updateDecl =
    funDecl
        Nothing
        updateAnn
        "update"
        []
        updateBody

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
        (Just <| exposeExplicit [ closedTypeExpose "Url" ])


importUrlParser : Import
importUrlParser =
    importStmt
        [ "Url", "Parser" ]
        Nothing
        (Just <| exposeExplicit [ funExpose "main" ])
