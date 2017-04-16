module Runner exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href, classList)
import Html.Events exposing (onInput, onSubmit)


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


init : ( Model, Cmd Msg )
init =
    ( { id = ""
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
    , Cmd.none
    )


type Msg
    = NameInput String
    | LocationInput String
    | AgeInput String
    | BibInput String
    | Save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model
                | name = name
                , nameError = Nothing
              }
            , Cmd.none
            )

        LocationInput location ->
            ( { model
                | location = location
                , locationError = Nothing
              }
            , Cmd.none
            )

        AgeInput age ->
            ( { model
                | age = age
                , ageError = Nothing
              }
            , Cmd.none
            )

        BibInput bib ->
            ( { model
                | bib = bib
                , bibError = Nothing
              }
            , Cmd.none
            )

        Save ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "box" ]
            [ h1 [ class "title" ] [ text "Add runner" ]
            , errorPanel model.error
            , form [ onSubmit Save ]
                [ textInputField
                    "Name"
                    [ value model.name
                    , class "greetings"
                    , onInput NameInput
                    , placeholder "Name"
                    ]
                    (Maybe.withDefault
                        ""
                        model.nameError
                    )
                , textInputField
                    "Location"
                    [ value model.location
                    , onInput LocationInput
                    , placeholder "Location"
                    ]
                    (Maybe.withDefault
                        ""
                        model.locationError
                    )
                , numberInputField
                    "Age"
                    [ value model.age
                    , onInput AgeInput
                    , placeholder "Age"
                    , Html.Attributes.min "0"
                    ]
                , numberInputField
                    "Bib #"
                    [ value model.bib
                    , onInput BibInput
                    , placeholder "Bib #"
                    , Html.Attributes.min "0"
                    ]
                , buttonInputField
                    "Save"
                    [ class "button is-primary"
                    , type_ "button"
                    , value "Save"
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


textInputField : String -> List (Html.Attribute Msg) -> String -> Html Msg
textInputField label attr error =
    let
        cls =
            [ ( "input", True ), ( "is-danger", not (String.isEmpty error) ) ]
    in
        div [ class "field" ]
            [ Html.label [ class "label" ] [ text label ]
            , p [ class "control" ]
                [ input
                    ((classList cls) :: (type_ "text") :: attr)
                    []
                ]
            ]


numberInputField : String -> List (Html.Attribute Msg) -> Html Msg
numberInputField label attr =
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text label ]
        , p [ class "control" ]
            [ input
                ((class "input") :: (type_ "number") :: attr)
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
