module Writer.OnePage exposing (write)

import Data exposing (..)
import Elm.CodeGen as CodeGen exposing (..)
import Elm.Pretty as Pretty


write : PageOptions -> List ( String, String )
write options =
    [ ( "Main.elm", mainFile options )
    ]


type ModuleType
    = UnknownModuleType
    | JustHtml
    | BrowserSandbox
    | BrowserElement


check : List Bool -> Bool
check =
    List.all identity


moduleType : PageOptions -> ModuleType
moduleType { initType, mainType, updateType, viewType, subscriptionType } =
    let
        justHtml =
            [ xor (mainType == Main1) (viewType == View1)
            , initType == Init0
            , updateType == Update0
            , subscriptionType == Subscription0
            ]

        browserSandbox =
            [ initType == Init1
            , mainType == Main0
            , updateType == Update3
            , viewType == View1 || viewType == View2
            , subscriptionType == Subscription0
            ]

        browserElement =
            [ initType == Init2
            , mainType == Main0
            , updateType == Update4
            , viewType == View1 || viewType == View2
            , [ Subscription0, Subscription1, Subscription2 ] |> List.member subscriptionType
            ]
    in
    if check browserElement then
        BrowserElement

    else if check browserSandbox then
        BrowserSandbox

    else if check justHtml then
        JustHtml

    else
        UnknownModuleType


hasModel : ModuleType -> PageOptions -> Bool
hasModel moduleType_ _ =
    case moduleType_ of
        JustHtml ->
            False

        BrowserSandbox ->
            True

        BrowserElement ->
            True

        UnknownModuleType ->
            False


hasMsg : ModuleType -> PageOptions -> Bool
hasMsg moduleType_ _ =
    case moduleType_ of
        JustHtml ->
            False

        BrowserSandbox ->
            True

        BrowserElement ->
            True

        UnknownModuleType ->
            False


hasInit : ModuleType -> PageOptions -> Bool
hasInit moduleType_ _ =
    case moduleType_ of
        JustHtml ->
            False

        BrowserSandbox ->
            True

        BrowserElement ->
            True

        UnknownModuleType ->
            False


hasMain : ModuleType -> PageOptions -> Bool
hasMain moduleType_ { mainType } =
    case moduleType_ of
        JustHtml ->
            mainType == Main1

        BrowserSandbox ->
            False

        BrowserElement ->
            False

        UnknownModuleType ->
            False


hasUpdate : ModuleType -> PageOptions -> Bool
hasUpdate moduleType_ _ =
    case moduleType_ of
        JustHtml ->
            False

        BrowserSandbox ->
            True

        BrowserElement ->
            True

        UnknownModuleType ->
            False


hasView : ModuleType -> PageOptions -> Bool
hasView moduleType_ { viewType } =
    case moduleType_ of
        JustHtml ->
            viewType == View1

        BrowserSandbox ->
            True

        BrowserElement ->
            True

        UnknownModuleType ->
            False


hasSubscriptions : ModuleType -> PageOptions -> Bool
hasSubscriptions moduleType_ { subscriptionType } =
    case moduleType_ of
        JustHtml ->
            False

        BrowserSandbox ->
            False

        BrowserElement ->
            not ([ Subscription0, SubscriptionUnknown ] |> List.member subscriptionType)

        UnknownModuleType ->
            False


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


add : a -> List a -> List a
add x xs =
    xs ++ [ x ]


addIf : Bool -> a -> List a -> List a
addIf cond x xs =
    if cond then
        add x xs

    else
        xs


appendIf : Bool -> List a -> List a -> List a
appendIf cond xs2 xs1 =
    if cond then
        xs1 ++ xs2

    else
        xs1



-- module App.Main exposing (main)
-- import Browser
-- import Main exposing (Model, Msg, init, update, view, subscriptions)


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
