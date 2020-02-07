module App.Page exposing (Model, Msg, enterRoute, init, subscriptions, update, view)

import App.Routes as Routes
import App.Utils exposing (mapDocument)
import Browser
import Html exposing (Html, text)
import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index



-- MODEL


type alias Model =
    { index : Maybe ()
    , about : Maybe ()
    , counter : Maybe Pages.Counter.Model
    , counterAsync : Maybe Pages.CounterAsync.Model
    }


init : Routes.Route -> ( Model, Cmd Msg )
init route =
    let
        model =
            { index = Nothing
            , about = Nothing
            , counter = Nothing
            , counterAsync = Nothing
            }
    in
    enterRoute model route


enterRoute : Model -> Routes.Route -> ( Model, Cmd Msg )
enterRoute model route =
    case route of
        Routes.NotFound ->
            ( model
            , Cmd.none
            )

        Routes.Index ->
            ( { model
                | index =
                    model.index
                        |> Maybe.withDefault ()
                        |> Just
              }
            , Cmd.none
            )

        Routes.About ->
            ( { model
                | about =
                    model.about
                        |> Maybe.withDefault ()
                        |> Just
              }
            , Cmd.none
            )

        Routes.Counter ->
            let
                pageModel =
                    Pages.Counter.init
            in
            ( { model
                | counter =
                    model.counter
                        |> Maybe.withDefault pageModel
                        |> Just
              }
            , Cmd.none
            )

        Routes.CounterAsync ->
            let
                ( pageModel, pageCmd ) =
                    Pages.CounterAsync.init
            in
            ( { model
                | counterAsync =
                    model.counterAsync
                        |> Maybe.withDefault pageModel
                        |> Just
              }
            , Cmd.map CounterAsyncMsg pageCmd
            )



-- MESSAGES


type Msg
    = IndexMsg ()
    | AboutMsg ()
    | CounterMsg Pages.Counter.Msg
    | CounterAsyncMsg Pages.CounterAsync.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexMsg _ ->
            ( model, Cmd.none )

        AboutMsg _ ->
            ( model, Cmd.none )

        CounterMsg subMsg ->
            let
                pageModel =
                    updateHelper1 Pages.Counter.update subMsg model.counter
            in
            ( { model | counter = pageModel }
            , Cmd.none
            )

        CounterAsyncMsg subMsg ->
            let
                ( pageModel, pageCmd ) =
                    updateHelper2 Pages.CounterAsync.update subMsg model.counterAsync
            in
            ( { model | counterAsync = pageModel }
            , Cmd.map CounterAsyncMsg pageCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ model.counterAsync
        |> Maybe.map Pages.CounterAsync.subscriptions
        |> Maybe.map (Sub.map CounterAsyncMsg)
    ]
        |> List.filterMap identity
        |> Sub.batch



-- VIEW


view : Routes.Route -> Model -> Browser.Document Msg
view route model =
    case route of
        Routes.NotFound ->
            { title = "404"
            , body = [ text "Not Found" ]
            }

        Routes.Index ->
            model.index
                |> Maybe.map (always Pages.Index.view)
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument (always (IndexMsg ()))

        Routes.About ->
            model.about
                |> Maybe.map (always Pages.About.view)
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument (always (AboutMsg ()))

        Routes.Counter ->
            model.counter
                |> Maybe.map Pages.Counter.view
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument CounterMsg

        Routes.CounterAsync ->
            model.counterAsync
                |> Maybe.map Pages.CounterAsync.view
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument CounterAsyncMsg


viewEmpty : Browser.Document msg
viewEmpty =
    { title = ""
    , body = [ text "" ]
    }


toDocument : String -> Html msg -> Browser.Document msg
toDocument title body =
    { title = title
    , body = [ body ]
    }



-- UTILS


updateHelper1 : (msg -> model -> model) -> msg -> Maybe model -> Maybe model
updateHelper1 f msg =
    Maybe.map (f msg)


updateHelper2 : (msg -> model -> ( model, Cmd msg )) -> msg -> Maybe model -> ( Maybe model, Cmd msg )
updateHelper2 f msg maybeModel =
    case maybeModel of
        Nothing ->
            ( Nothing, Cmd.none )

        Just model ->
            f msg model
                |> Tuple.mapFirst Just
