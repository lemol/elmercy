port module Main exposing (main)

import Task
import Writer



-- DATA


type alias Config =
    { plugins : List String
    }


type alias FileList =
    List ( String, String )



-- PROGRAM


main : Program () Model Msg
main =
    Platform.worker
        { init = always init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- MESSAGES


type Msg
    = GotConfig Config
    | Flush FileList



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotConfig config ->
            ( model
            , Writer.createFiles config.plugins
                |> Flush
                |> dispatch
            )

        Flush list ->
            ( model
            , flushResult list
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ initConfig (Config >> GotConfig)
        ]



-- PORTS


port initConfig : (List String -> msg) -> Sub msg


port flushResult : List ( String, String ) -> Cmd msg



-- UTILS


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity
