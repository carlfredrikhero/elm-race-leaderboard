module Leaderboard exposing (..)

import Html exposing (Html, form, div, input, table, tr, td, thead, tbody, th, text)
import Html.Attributes exposing (type_, placeholder, value, class)
import Html.Events exposing (onInput)


-- model


type alias Model =
    { runners : List Runner
    , query : String
    }


initModel : Model
initModel =
    { runners = [ Runner "Algot" ]
    , query = ""
    }


type alias Runner =
    { name : String
    }


type Msg
    = QueryInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        QueryInput query ->
            { model | query = query }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "section search-section" ]
            [ input
                [ class "input"
                , type_ "search"
                , placeholder "Search..."
                , value model.query
                , onInput QueryInput
                ]
                []
            ]
        , div [ class "section" ]
            [ table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Runner name" ]
                        ]
                    ]
                , tbody []
                    (List.map runnerRow model.runners)
                ]
            ]
        ]


runnerRow : Runner -> Html Msg
runnerRow runner =
    tr []
        [ td [] [ text runner.name ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
