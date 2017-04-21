module Runner exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button, nav, a)
import Html.Attributes exposing (type_, placeholder, value, class, href, classList)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing (field)


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


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )


type Msg
    = NameInput String
    | LocationInput String
    | AgeInput String
    | BibInput String
    | Save
    | SaveResponse (Result Http.Error String)


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update token msg model =
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
            let
                updatedModel =
                    validate model
            in
                if isValid updatedModel then
                    save token updatedModel
                else
                    ( updatedModel, Cmd.none )

        SaveResponse (Ok id) ->
            ( initModel, Cmd.none )

        SaveResponse (Err err) ->
            let
                errMsg =
                    case err of
                        Http.BadStatus resp ->
                            resp.body

                        _ ->
                            "Error Saving!"
            in
                ( { model | error = Just errMsg }, Cmd.none )


url : String
url =
    "http://localhost:5000/runner"


save : String -> Model -> ( Model, Cmd Msg )
save token model =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ token) ]

        body =
            Http.jsonBody <| runnerEncoder model

        decoder =
            field "_id" JD.string

        request =
            post url headers body decoder

        cmd =
            Http.send SaveResponse request
    in
        ( model, cmd )


runnerEncoder : Model -> JE.Value
runnerEncoder { name, location, age, bib } =
    let
        ageInt =
            age
                |> String.toInt
                |> Result.withDefault 0

        bibInt =
            bib
                |> String.toInt
                |> Result.withDefault 0
    in
        JE.object
            [ ( "name", JE.string name )
            , ( "location", JE.string location )
            , ( "age", JE.int ageInt )
            , ( "bib", JE.int bibInt )
            ]


post : String -> List Http.Header -> Http.Body -> JD.Decoder a -> Http.Request a
post url headers body decoder =
    Http.request
        { method = "POST"
        , url = url
        , headers = headers
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


isValid : Model -> Bool
isValid model =
    model.nameError
        == Nothing
        && model.locationError
        == Nothing
        && model.ageError
        == Nothing
        && model.bibError
        == Nothing


validate : Model -> Model
validate model =
    model
        |> validateName
        |> validateLocation
        |> validateAge
        |> validateBib


validateName : Model -> Model
validateName model =
    if String.isEmpty model.name then
        { model
            | nameError = Just "Name is required"
        }
    else
        { model
            | nameError = Nothing
        }


validateLocation : Model -> Model
validateLocation model =
    if String.isEmpty model.location then
        { model
            | locationError = Just "Location is required"
        }
    else
        { model
            | locationError = Nothing
        }


validateAge : Model -> Model
validateAge model =
    let
        ageInt =
            model.age
                |> String.toInt
                |> Result.withDefault 0
    in
        if ageInt <= 0 then
            { model
                | ageError = Just "Age must be a positive number"
            }
        else
            { model
                | ageError = Nothing
            }


validateBib : Model -> Model
validateBib model =
    let
        bibInt =
            model.bib
                |> String.toInt
                |> Result.withDefault 0
    in
        if bibInt <= 0 then
            { model
                | bibError = Just "Bib must be a positive number"
            }
        else
            { model
                | bibError = Nothing
            }


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
                , submitInputField
                    [ class "button is-primary"
                    , type_ "submit"
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
            , p [ class "help is-danger" ] [ text error ]
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
