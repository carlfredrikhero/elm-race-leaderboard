module Leaderboard exposing (..)

import Html exposing (Html, form, div, input, table, tr, td, thead, tbody, th, text, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href, id)
import Html.Events exposing (onInput, onClick)
import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import WebSocket exposing (..)
import Time exposing (every, second)
import Date
import Date.Extra.Format as DateFormat
import Date.Extra.Config.Config_en_us as DateConfig


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
    | Tick Time.Time
    | ToggleActive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )

        WsMessage wsMsg ->
            wsMessage wsMsg model

        Tick time ->
            ( tick time model, Cmd.none )

        ToggleActive ->
            ( { model | active = not model.active }, Cmd.none )


tick : Float -> Model -> Model
tick time model =
    let
        updatedRunners =
            List.map (advanceDistance time) model.runners
    in
        { model | runners = updatedRunners }


advanceDistance : Float -> Runner -> Runner
advanceDistance time runner =
    let
        elapsedMinutes =
            (time - runner.lastMarkerTime) / 1000 / 60
    in
        if runner.lastMarkerTime > 0 then
            { runner
                | estimatedDistance =
                    runner.lastMarkerDistance + (runner.pace * elapsedMinutes)
            }
        else
            runner


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

                "update runner" ->
                    let
                        updatedRunners =
                            List.map
                                (\r ->
                                    if r.id == runner.id then
                                        runner
                                    else
                                        r
                                )
                                model.runners
                    in
                        ( { model
                            | runners = updatedRunners
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
            , toggleActiveLink model.active
            , searchPanel model.query
            ]
        , div [ class "section" ]
            [ runners model
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


runners : Model -> Html Msg
runners { query, runners } =
    runners
        |> List.filter (\r -> String.contains query r.name)
        |> List.sortWith descComparison
        |> List.map runner
        |> tbody []
        |> (\r -> runnersHeader :: [ r ])
        |> table [ class "table" ]


descComparison : Runner -> Runner -> Order
descComparison a b =
    case compare a.estimatedDistance b.estimatedDistance of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


runnersHeader : Html Msg
runnersHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "From" ]
            , th [] [ text "Age" ]
            , th [] [ text "Bib #" ]
            , th [] [ text "Last Marker" ]
            , th [] [ text "Est. Miles" ]
            ]
        ]


toggleActiveLink : Bool -> Html Msg
toggleActiveLink active =
    let
        label =
            if active then
                "Pause realtime updates"
            else
                "Restore realtime updates"
    in
        a
            [ class "nav-item"
            , onClick (ToggleActive)
            ]
            [ text label ]


lastMarker : Runner -> Html Msg
lastMarker runner =
    if runner.lastMarkerTime > 0 then
        text
            (formatDistance runner.lastMarkerDistance
                ++ " mi @ "
                ++ (formatTime runner.lastMarkerTime)
            )
    else
        text ""


formatTime : Time.Time -> String
formatTime time =
    if time > 0 then
        time
            |> Date.fromTime
            |> DateFormat.format DateConfig.config "%H:%M:%S %P"
    else
        ""


runner : Runner -> Html Msg
runner runner =
    tr []
        [ td [] [ text runner.name ]
        , td [] [ text runner.location ]
        , td [] [ text (toString runner.age) ]
        , td [] [ text (toString runner.bib) ]
        , td [] [ lastMarker runner ]
        , td [] [ text (formatDistance runner.estimatedDistance) ]
        ]


formatDistance : Float -> String
formatDistance distance =
    if distance <= 0 then
        toString 0
    else
        distance
            * 100
            |> round
            |> toFloat
            |> flip (/) 100
            |> toString


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.active then
        Sub.batch
            [ listen url WsMessage
            , every second Tick
            ]
    else
        Sub.none
