port module Main exposing (main)

import Data exposing (App(..), AppType(..))
import Maybe.Extra
import Parser
import Task
import Writer



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
    { app : App
    , appType : AppType
    , pendingFiles : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { app = Empty
      , appType = Unknown
      , pendingFiles = []
      }
    , requestSourcePaths ()
    )



-- MESSAGES


type Msg
    = GotSourcePaths (List String)
    | GotSourceCode ( String, String )
    | WriteApp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSourcePaths paths ->
            let
                ( appType, pendingFiles, cmd ) =
                    processFiles paths
            in
            ( { model
                | appType = appType
                , pendingFiles = pendingFiles
              }
            , cmd
            )

        GotSourceCode ( path, source ) ->
            let
                newApp =
                    Parser.parseNextSource model.appType model.app source
                        |> Result.withDefault model.app

                nextCmd =
                    model.pendingFiles
                        |> List.head
                        |> Maybe.map requestSourceCode
                        |> Maybe.withDefault (send WriteApp)

                pendingFiles =
                    model.pendingFiles
                        |> List.filter ((/=) path)
            in
            ( { model | app = newApp, pendingFiles = pendingFiles }
            , nextCmd
            )

        WriteApp ->
            let
                result =
                    Writer.getResult model.app
            in
            ( model
            , writeResult result
            )


processFiles : List String -> ( AppType, List String, Cmd Msg )
processFiles paths =
    let
        checkSingleFile =
            if not (paths |> List.any (String.startsWith "Pages/")) then
                let
                    mainFile =
                        [ "Index.elm"
                        , "Main.elm"
                        ]
                            |> List.filter (\x -> List.member x paths)
                            |> List.head
                in
                Just (\x -> ( SimpleHtmlAppType, [], requestSourceCode x ))
                    |> Maybe.Extra.andMap mainFile

            else
                Nothing
    in
    [ checkSingleFile ]
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault ( Unknown, [], Cmd.none )
        |> identity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ readSourcePaths GotSourcePaths
        , readSourceCode GotSourceCode
        ]



-- PORTS


port requestSourcePaths : () -> Cmd msg


port requestSourceCode : String -> Cmd msg


port writeResult : List ( String, String ) -> Cmd msg


port readSourcePaths : (List String -> msg) -> Sub msg


port readSourceCode : (( String, String ) -> msg) -> Sub msg



-- UTILS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- log : a -> a
-- log a =
--     Debug.log (Debug.toString a) a
