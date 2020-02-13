module Writer.Utils exposing (..)

import Data exposing (..)


type ModuleType
    = UnknownModuleType
    | JustHtml
    | BrowserSandbox
    | BrowserElement



-- LIST EXTRA


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



-- DATA


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



-- EXISTENCE


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



-- HELPERS


check : List Bool -> Bool
check =
    List.all identity
