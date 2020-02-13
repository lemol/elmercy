module Writer.MultiplePages.DataModule exposing (write)

import Data exposing (SubscriptionType(..))
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty


write : String
write =
    let
        moduleName =
            [ "App", "Data" ]

        module_ =
            normalModule moduleName
                [ typeOrAliasExpose "Model"
                , funExpose "init"
                ]

        importList =
            [ importStmt [ "App", "Routes" ] (Just [ "Routes" ]) Nothing
            , importStmt [ "Browser", "Navigation" ] (Just [ "Navigation" ]) Nothing
            ]

        declarationList =
            [ modelDecl
            , initDecl
            ]

        -- DECLARATIONS
        modelDecl =
            aliasDecl Nothing "Model" [] <|
                recordAnn
                    [ ( "navigationKey"
                      , typed "Navigation.Key" []
                      )
                    , ( "route"
                      , typed "Routes.Route" []
                      )
                    ]

        initDecl =
            funDecl
                Nothing
                (Just <| funAnn (typed "Navigation.Key" []) (funAnn (typed "Routes.Route" []) (typed "Model" [])))
                "init"
                [ varPattern "key", varPattern "route" ]
                (record [ ( "navigationKey", val "key" ), ( "route", val "route" ) ])
    in
    CodeGen.file module_ importList declarationList Nothing
        |> Pretty.pretty 100
