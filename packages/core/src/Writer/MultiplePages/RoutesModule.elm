module Writer.MultiplePages.RoutesModule exposing (write)

import Data exposing (AppPage, SubscriptionType(..))
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty


write : List AppPage -> String
write pages =
    let
        moduleName =
            [ "App", "Routes" ]

        module_ =
            normalModule moduleName
                [ openTypeExpose "Route"
                , funExpose "parseUrl"
                , funExpose "toPath"
                ]

        importList =
            [ importStmt [ "Url" ] Nothing (Just <| exposeExplicit [ closedTypeExpose "Url" ])
            , importStmt
                [ "Url", "Parser" ]
                Nothing
                (Just <| exposeExplicit (closedTypeExpose "Parser" :: List.map funExpose [ "map", "oneOf", "parse", "s", "top" ]))
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
                    |> List.map (\x -> ( x.routeName, [] ))
                )

        parseUrlDecl =
            funDecl
                Nothing
                (Just <| funAnn (typed "Url" []) (typed "Route" []))
                "parseUrl"
                [ varPattern "url" ]
            <|
                letExpr
                    [ letFunction "newUrl" [] (update "url" [ ( "path", applyBinOp (access (val "url") "fragment") piper (apply [ fqFun [ "Maybe" ] "withDefault", string "" ]) ) ]) ]
                    (caseExpr (apply [ val "parse", val "matchRoute", val "newUrl" ])
                        [ ( namedPattern "Just" [ varPattern "route" ]
                          , val "route"
                          )
                        , ( namedPattern "Nothing" []
                          , val "NotFound"
                          )
                        ]
                    )

        toPathDecl =
            funDecl
                Nothing
                (Just <| funAnn (typed "Route" []) (typed "String" []))
                "toPath"
                [ varPattern "route" ]
            <|
                letExpr
                    [ letFunction "prefix" [] (string "#")
                    , letFunction "path" [] <|
                        caseExpr (val "route")
                            (pages
                                |> List.map
                                    (\x ->
                                        ( namedPattern x.routeName []
                                        , string x.routePath
                                        )
                                    )
                            )
                    ]
                    (applyBinOp (val "prefix") append (val "path"))

        matchRouteDecl =
            funDecl Nothing
                (Just <| typed "Parser" [ funAnn (typed "Route" []) (typeVar "a"), typeVar "a" ])
                "matchRoute"
                []
            <|
                apply
                    [ val "oneOf"
                    , pages
                        |> List.map
                            (\x ->
                                apply
                                    [ val "map"
                                    , val x.routeName
                                    , parens <|
                                        apply
                                            [ val "s"
                                            , if x.routePath == "/" then
                                                val "top"

                                              else
                                                string x.routePath
                                            ]
                                    ]
                            )
                        |> list
                    ]
    in
    CodeGen.file module_ importList declarationList Nothing
        |> Pretty.pretty 100
