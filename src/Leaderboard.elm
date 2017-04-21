module Leaderboard exposing (..)

import Html exposing (Html, form, div, input, table, tr, td, thead, tbody, th, text, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href, id)
import Html.Events exposing (onInput)
import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import WebSocket exposing (..)


-- model


type alias Model =
    { error : Maybe String
    , runners : List Runner
    , query : String
    , active : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { error = Nothing
      , runners = []
      , query = ""
      , active = True
      }
    , listenRunnerCmd
    )


url : String
url =
    "ws://localhost:5000/runners"


listenRunnerCmd : Cmd Msg
listenRunnerCmd =
    send url (encodeMsg "listen runners" JE.null)


encodeMsg : String -> JE.Value -> String
encodeMsg name data =
    JE.object
        [ ( "name", JE.string name )
        , ( "data", data )
        ]
        |> JE.encode 0


type alias Runner =
    { id : String
    , name : String
    , location : String
    , age : Int
    , bib : Int
    , estimatedDistance : Float
    , lastMarkerDistance : Float
    , lastMarkerTime : Float
    , pace : Float
    }


type alias RunnerWsMsg =
    { name : String
    , runner : Runner
    }


type Msg
    = SearchInput String
    | WsMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        WsMessage wsMsg ->
            wsMessage wsMsg model


wsMessage : String -> Model -> ( Model, Cmd Msg )
wsMessage wsMsg model =
    case JD.decodeString msgDecoder wsMsg of
        Ok { name, runner } ->
            case name of
                "new runner" ->
                    ( { model
                        | runners = runner :: model.runners
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | error = Just ("Unrecognized message: " ++ name)
                      }
                    , Cmd.none
                    )

        Err err ->
            ( { model
                | error = Just err
              }
            , Cmd.none
            )


msgDecoder : JD.Decoder RunnerWsMsg
msgDecoder =
    JDP.decode RunnerWsMsg
        |> JDP.required "name" JD.string
        |> JDP.required "data" runnerDecoder


runnerDecoder : JD.Decoder Runner
runnerDecoder =
    JDP.decode Runner
        |> JDP.required "_id" JD.string
        |> JDP.required "name" JD.string
        |> JDP.required "location" JD.string
        |> JDP.required "age" JD.int
        |> JDP.required "bib" JD.int
        |> JDP.hardcoded 0
        |> JDP.required "lastMarkerDistance" JD.float
        |> JDP.required "lastMarkerTime" JD.float
        |> JDP.required "pace" JD.float


view : Model -> Html Msg
view model =
    div [ id "leaderboard" ]
        [ div [ class "section" ]
            [ errorPanel model.error
            , searchPanel model.query
            ]
        , div [ class "section" ]
            [ runnersPanel model
            ]
        ]


searchPanel : String -> Html Msg
searchPanel query =
    input
        [ class "input"
        , type_ "search"
        , placeholder "Search..."
        , value query
        , onInput SearchInput
        ]
        []


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "message is-danger" ]
                [ text msg
                ]


runnersPanel : Model -> Html Msg
runnersPanel model =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ]
                , th [] [ text "Name" ]
                , th [] [ text "Location" ]
                , th [] [ text "Bib" ]
                , th [] [ text "Est. distance" ]
                , th [] [ text "Last marker distance" ]
                , th [] [ text "Last marker time" ]
                , th [] [ text "Pace" ]
                ]
            ]
        , tbody []
            (List.map runnerRow model.runners)
        ]


runnerRow : Runner -> Html Msg
runnerRow runner =
    tr []
        [ td [] [ text runner.id ]
        , td [] [ text runner.name ]
        , td [] [ text runner.location ]
        , td [] [ text (toString runner.bib) ]
        , td [] [ text (toString runner.estimatedDistance) ]
        , td [] [ text (toString runner.lastMarkerDistance) ]
        , td [] [ text (toString runner.lastMarkerTime) ]
        , td [] [ text (toString runner.pace) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    listen url WsMessage
