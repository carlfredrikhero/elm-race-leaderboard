module Main exposing (..)

import Html exposing (Html, div, nav, a, span, text, hr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Leaderboard
import Login


-- model


type alias Model =
    { page : Page
    , leaderboard : Leaderboard.Model
    , login : Login.Model
    }


initModel : Model
initModel =
    { page = Leaderboard
    , leaderboard = Leaderboard.initModel
    , login = Login.initModel
    }


type Page
    = Leaderboard
    | Login



-- update


type Msg
    = ChangePage Page
    | LeaderboardMsg Leaderboard.Msg
    | LoginMsg Login.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangePage page ->
            { model | page = page }

        LeaderboardMsg lbMsg ->
            { model | leaderboard = (Leaderboard.update lbMsg model.leaderboard) }

        LoginMsg lgMsg ->
            { model | login = (Login.update lgMsg model.login) }



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                Leaderboard ->
                    Html.map LeaderboardMsg
                        (Leaderboard.view model.leaderboard)

                Login ->
                    Html.map LoginMsg
                        (Login.view model.login)
    in
        div []
            [ nav []
                [ a
                    [ href "#"
                    , onClick (ChangePage Leaderboard)
                    ]
                    [ text "Leaderboard" ]
                , span [] [ text " | " ]
                , a
                    [ href "#"
                    , onClick (ChangePage Login)
                    ]
                    [ text "Login" ]
                ]
            , hr [] []
            , page
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
