module Leaderboard exposing (..)

import Html exposing (Html, form, div, input, table, tr, td, thead, tbody, th, text, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href)
import Html.Events exposing (onInput)


-- model


type alias Model =
    { error : Maybe String
    , runners : List Runner
    , query : String
    , active : Bool
    }


initModel : Model
initModel =
    { error = Nothing
    , runners = tempRunners
    , query = ""
    , active = True
    }


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


tempRunners : List Runner
tempRunners =
    [ Runner "1" "James Moore" "Turlock CA" 42 1234 0 1 1463154945381 0.125
    , Runner "2" "Meb Keflezighi" "Turlock CA" 41 1238 0 1 1463154945381 0.09
    ]


type Msg
    = SearchInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SearchInput query ->
            { model | query = query }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewHeader model
        , div [ class "section" ]
            [ errorPanel model.error
            , searchPanel model.query
            ]
        , div [ class "section" ]
            [ runnersPanel model
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    nav [ class "nav hero is-default" ]
        [ div [ class "container" ]
            [ a [ class "nav-item logo" ] [ text "Race Results" ]
            , a
                [ class "nav-item"
                , href "#"
                ]
                [ text "Leaderboard" ]
            , a
                [ class "nav-item"
                , href "#"
                ]
                [ text "Login" ]
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


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
