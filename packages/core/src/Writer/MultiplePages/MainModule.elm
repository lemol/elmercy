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
            , parseUrlDecl
            , toPathDecl
            , matchRouteDecl
            ]

        -- DECLARATIONS
        routeDecl =
            customTypeDecl Nothing
                "Route"
                []
                (pages
                    |> List.map routeConst
                )

        parseUrlAnn =
            funAnn (typed "Url" []) (typed "Route" [])

        parseUrlBody =
            letExpr
                [ letFunction "newUrl"
                    []
                    (update "url"
                        [ ( "path"
                          , applyBinOp (access (val "url") "fragment")
                                piper
                                (apply
                                    [ fqFun [ "Maybe" ] "withDefault"
                                    , string ""
                                    ]
                                )
                          )
                        ]
                    )
                ]
                (caseExpr
                    (apply [ val "parse", val "matchRoute", val "newUrl" ])
                    [ ( namedPattern "Just" [ varPattern "route" ]
                      , val "route"
                      )
                    , ( namedPattern "Nothing" []
                      , val config.notFound.routeName
                      )
                    ]
                )

        parseUrlDecl =
            funDecl
                Nothing
                (Just parseUrlAnn)
                "parseUrl"
                [ varPattern "url" ]
                parseUrlBody

        toPathAnn =
            funAnn (typed "Route" []) (typed "String" [])

        toPathBody =
            letExpr
                [ letFunction "prefix" [] (string "#")
                , letFunction "path"
                    []
                    (caseExpr
                        (val "route")
                        (List.map routePathPair pages)
                    )
                ]
                (applyBinOp (val "prefix") append (val "path"))

        toPathDecl =
            funDecl
                Nothing
                (Just toPathAnn)
                "toPath"
                [ varPattern "route" ]
                toPathBody

        matchRouteAnn =
            typed "Parser"
                [ funAnn (typed "Route" []) (typeVar "a")
                , typeVar "a"
                ]

        matchRouteBody =
            apply
                [ val "oneOf"
                , pages
                    |> List.map routeMap
                    |> list
                ]

        matchRouteDecl =
            funDecl Nothing
                (Just matchRouteAnn)
                "matchRoute"
                []
                matchRouteBody
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
