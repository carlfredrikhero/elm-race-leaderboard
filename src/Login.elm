module Login exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href, method)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Navigation exposing (newUrl)


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


init : ( Model, Cmd Msg, Maybe String )
init =
    ( initModel
    , Cmd.none
    , Nothing
    )



-- update


url : String
url =
    "http://localhost:5000/authenticate"


type Msg
    = UsernameInput String
    | PasswordInput String
    | Submit
    | LoginResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none, Nothing )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            let
                body =
                    JE.object
                        [ ( "username", JE.string model.username )
                        , ( "password", JE.string model.password )
                        ]
                        |> JE.encode 4
                        |> Http.stringBody "application/json"

                request =
                    Http.post url body decoder

                decoder =
                    field "token" JD.string

                cmd =
                    Http.send LoginResponse request
            in
                ( model, cmd, Nothing )

        LoginResponse (Ok token) ->
            ( initModel, Navigation.newUrl "#/", Just token )

        LoginResponse (Err err) ->
            let
                errMsg =
                    case err of
                        Http.BadStatus resp ->
                            case resp.status.code of
                                401 ->
                                    resp.body

                                _ ->
                                    resp.status.message

                        _ ->
                            "Login Error!"
            in
                ( { model | error = Just errMsg }, Cmd.none, Nothing )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "columns is-vcentered" ]
            [ div [ class "column is-4 is-offset-4" ]
                [ h1 [ class "title" ] [ text "Login" ]
                , div [ class "box" ]
                    [ errorPanel model.error
                    , form [ method "POST", onSubmit Submit ]
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
                        , submitInputField
                            [ class "button is-primary"
                            , type_ "submit"
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


submitInputField : List (Html.Attribute Msg) -> Html Msg
submitInputField attr =
    div [ class "field" ]
        [ p [ class "control" ]
            [ input
                attr
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
