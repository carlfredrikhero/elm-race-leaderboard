module Login exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href)
import Html.Events exposing (onInput)


-- model


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    , error = Nothing
    }



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameInput username ->
            { model | username = username }

        PasswordInput password ->
            { model | password = password }

        Submit ->
            model

        Error error ->
            { model | error = Just error }



-- view


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [ class "section" ]
            [ loginForm model
            ]
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "notification is-danger" ]
                [ text msg
                ]


loginForm : Model -> Html Msg
loginForm model =
    div []
        [ div [ class "columns is-vcentered" ]
            [ div [ class "column is-4 is-offset-4" ]
                [ h1 [ class "title" ] [ text "Login for now..." ]
                , div [ class "box" ]
                    [ errorPanel model.error
                    , form []
                        [ textInputField
                            "Username"
                            [ class "input"
                            , type_ "text"
                            , onInput UsernameInput
                            , value model.username
                            , placeholder "Username"
                            ]
                        , textInputField
                            "Password"
                            [ class "input"
                            , type_ "password"
                            , onInput PasswordInput
                            , value model.password
                            , placeholder "Password"
                            ]
                        , buttonInputField
                            "Log in"
                            [ class "button is-primary"
                            , type_ "button"
                            , value "Log in"
                            ]
                        ]
                    ]
                ]
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


textInputField : String -> List (Html.Attribute Msg) -> Html Msg
textInputField label attr =
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text label ]
        , p [ class "control" ]
            [ input
                attr
                []
            ]
        ]


buttonInputField : String -> List (Html.Attribute Msg) -> Html Msg
buttonInputField label attr =
    div [ class "field" ]
        [ p [ class "control" ]
            [ button
                attr
                [ text label ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
