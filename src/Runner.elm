module Runner exposing (..)

import Html exposing (Html, form, input, div, h1, text, p, button)
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
        [ div [ class "section" ]
            []
        ]

main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }