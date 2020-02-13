module Writer.OnePage exposing (write)

import Data exposing (..)
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty
import Writer.Utils exposing (..)


write : PageOptions -> List ( String, String )
write options =
    [ ( "Main.elm", mainFile options )
    ]


mainFile : PageOptions -> String
mainFile options =
    let
        moduleType_ =
            moduleType options

        module_ =
            normalModule
                [ "App", "Main" ]
                [ funExpose "main" ]

        pageModuleImportExposing =
            []
                |> addIf (hasModel moduleType_ options) (closedTypeExpose "Model")
                |> addIf (hasMsg moduleType_ options) (closedTypeExpose "Msg")
                |> addIf (hasInit moduleType_ options) (funExpose "init")
                |> addIf (hasUpdate moduleType_ options) (funExpose "update")
                |> addIf (hasView moduleType_ options) (funExpose "view")
                |> addIf (hasSubscriptions moduleType_ options) (funExpose "subscriptions")

        importList =
            []
                |> addIf (moduleType_ == JustHtml)
                    (importStmt
                        [ "Html" ]
                        Nothing
                        (Just <| exposeExplicit [ closedTypeExpose "Html" ])
                    )
                |> addIf (moduleType_ == BrowserSandbox || moduleType_ == BrowserElement)
                    (importStmt
                        [ "Browser" ]
                        Nothing
                        Nothing
                    )
                |> add
                    (importStmt
                        [ options.moduleName ]
                        Nothing
                        (Just <| exposeExplicit pageModuleImportExposing)
                    )

        declarationList =
            [ mainDecl ]

        -- DECLARATIONS
        mainDef =
            if moduleType_ == JustHtml then
                { ann = Just <| typed "Html" [ typeVar "msg" ]
                , body =
                    apply
                        [ fqVal [ options.moduleName ]
                            (if hasMain moduleType_ options then
                                "main"

                             else
                                "view"
                            )
                        ]
                }

            else if moduleType_ == BrowserSandbox || moduleType_ == BrowserElement then
                { ann =
                    typed "Program"
                        [ unitAnn
                        , typed "Model" []
                        , typed "Msg" []
                        ]
                        |> Just
                , body =
                    apply
                        [ fqVal [ "Browser" ]
                            (if moduleType_ == BrowserSandbox then
                                "sandbox"

                             else if moduleType_ == BrowserElement then
                                "element"

                             else
                                "unknown_module_type"
                            )
                        , []
                            |> addIf (hasInit moduleType_ options) ( "init", initRecord options )
                            |> addIf (hasUpdate moduleType_ options) ( "update", updateRecord options )
                            |> addIf (hasView moduleType_ options) ( "view", viewRecord options )
                            |> addIf (hasSubscriptions moduleType_ options) ( "subscriptions", subscriptionsRecord options )
                            |> record
                        ]
                }

            else
                { ann = Nothing
                , body = apply [ fqVal [ options.moduleName ] "todo", string "<unknown module type>" ]
                }

        mainDecl =
            funDecl
                Nothing
                mainDef.ann
                "main"
                []
                mainDef.body
    in
    CodeGen.file module_ importList declarationList Nothing
        |> Pretty.pretty 100



-- UTILS


initRecord : PageOptions -> Expression
initRecord { initType } =
    if initType == Init1 then
        val "init"

    else if initType == Init2 then
        apply [ val "always", val "init" ]

    else
        apply [ fqVal [ "Debug" ] "todo", string "<unknown init>" ]


updateRecord : PageOptions -> Expression
updateRecord { updateType } =
    if updateType == Update3 || updateType == Update4 then
        val "update"

    else
        apply [ fqVal [ "Debug" ] "todo", string "<unknown update>" ]


viewRecord : PageOptions -> Expression
viewRecord { viewType } =
    if viewType == View1 then
        apply [ val "always", val "view" ]

    else if viewType == View2 then
        val "view"

    else
        apply [ fqVal [ "Debug" ] "todo", string "<unknown view>" ]


subscriptionsRecord : PageOptions -> Expression
subscriptionsRecord { subscriptionType } =
    if subscriptionType == Subscription1 then
        apply [ val "always", val "subscriptions" ]

    else if subscriptionType == Subscription2 then
        val "subscriptions"

    else
        apply [ fqVal [ "Debug" ] "todo", string "<unknown subscriptions>" ]
