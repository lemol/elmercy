module Writer.MultiplePages.RoutesModule exposing (write)

import Data exposing (AppConfig, AppPage, SubscriptionType(..))
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty


write : AppConfig -> List AppPage -> String
write config pages_ =
    let
        pages =
            pages_ ++ [ config.notFound ]

        module_ =
            normalModule
                [ "App", "Routes" ]
                [ openTypeExpose "Route"
                , funExpose "parseUrl"
                , funExpose "toPath"
                ]

        importList =
            [ importStmt
                [ "Url" ]
                Nothing
                (Just <| exposeExplicit [ closedTypeExpose "Url" ])
            , importStmt
                [ "Url", "Parser" ]
                Nothing
                ((Just
                    << exposeExplicit
                 )
                    (closedTypeExpose "Parser"
                        :: List.map funExpose [ "map", "oneOf", "parse", "s", "top" ]
                    )
                )
            ]

        declarationList =
            [ routeDecl
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


routeMap : { a | routeName : String, routePath : String } -> Expression
routeMap page =
    apply
        [ val "map"
        , val page.routeName
        , parens <|
            if page.routePath == "/" then
                val "top"

            else
                apply
                    [ val "s", string page.routePath ]
        ]


routePathPair : { a | routeName : String, routePath : String } -> ( Pattern, Expression )
routePathPair page =
    ( namedPattern page.routeName []
    , string page.routePath
    )


routeConst : { a | routeName : String, routePath : String } -> ( String, List TypeAnnotation )
routeConst page =
    ( page.routeName
    , []
    )
