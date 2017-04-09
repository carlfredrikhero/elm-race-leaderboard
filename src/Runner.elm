module Runner exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href)
import Html.Events exposing (onInput)


-- model


type alias Model =
    { id : String
    , name : String
    , nameError : Maybe String
    , location : String
    , locationError : Maybe String
    , age : String
    , ageError : Maybe String
    , bib : String
    , bibError : Maybe String
    , error : Maybe String
    }


initModel : Model
initModel =
    { id = ""
    , name = ""
    , nameError = Nothing
    , location = ""
    , locationError = Nothing
    , age = ""
    , ageError = Nothing
    , bib = ""
    , bibError = Nothing
    , error = Nothing
    }


type Msg
    = NameInput String
    | LocationInput String
    | AgeInput String
    | BibInput String
    | Save


update : Msg -> Model -> Model
update msg model =
    case msg of
        NameInput name ->
            { model
                | name = name
                , nameError = Nothing
            }

        LocationInput location ->
            { model
                | location = location
                , locationError = Nothing
            }

        AgeInput age ->
            { model
                | age = age
                , ageError = Nothing
            }

        BibInput bib ->
            { model
                | bib = bib
                , bibError = Nothing
            }

        Save ->
            model


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [ class "section" ]
            [ div [ class "container" ]
                [ runnerForm model
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


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "notification is-danger" ]
                [ text msg
                ]


runnerForm : Model -> Html Msg
runnerForm model =
    div []
        [ div [ class "box" ]
            [ h1 [ class "title" ] [ text "Add runner" ]
            , errorPanel model.error
            , form []
                [ textInputField
                    "Name"
                    [ class "input"
                    , type_ "text"
                    , value model.name
                    , placeholder "Name"
                    ]
                  -- , textInputField
                  --     "Password"
                  --     [ class "input"
                  --     , type_ "password"
                  --     , onInput PasswordInput
                  --     , value model.password
                  --     , placeholder "Password"
                  --     ]
                , buttonInputField
                    "Save"
                    [ class "button is-primary"
                    , type_ "button"
                    , value "Save"
                    ]
                ]
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
