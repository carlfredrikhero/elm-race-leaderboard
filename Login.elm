module Login exposing (..)

import Html exposing (Html, form, input, div, h3, text)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onInput)


-- model


type alias Model =
    { username : String
    , password : String
    }


initModel : Model
initModel =
    { username = ""
    , password = ""
    }



-- update


type Msg
    = UsernameInput String
    | PasswordInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameInput username ->
            { model | username = username }

        PasswordInput password ->
            { model | password = password }



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Login for now..." ]
        , form []
            [ input
                [ type_ "text"
                , onInput UsernameInput
                , value model.username
                , placeholder "username"
                ]
                []
            , input
                [ type_ "password"
                , onInput PasswordInput
                , value model.password
                , placeholder "password"
                ]
                []
            , input
                [ type_ "button"
                , value "Login"
                ]
                []
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
