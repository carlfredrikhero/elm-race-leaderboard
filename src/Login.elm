module Login exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href)
import Html.Events exposing (onInput, onSubmit)


-- model


type alias Model =
    { username : String
    , password : String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , password = ""
      , error = Nothing
      }
    , Cmd.none
    )



-- update


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( model, Cmd.none )

        Error error ->
            ( { model | error = Just error }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "columns is-vcentered" ]
            [ div [ class "column is-4 is-offset-4" ]
                [ h1 [ class "title" ] [ text "Login" ]
                , div [ class "box" ]
                    [ errorPanel model.error
                    , form [ onSubmit Submit ]
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


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "notification is-danger" ]
                [ text msg
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
