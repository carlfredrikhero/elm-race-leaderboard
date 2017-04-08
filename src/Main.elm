module Main exposing (..)

import Html exposing (Html, div, nav, a, span, text, header, h1)
import Html.Attributes exposing (href, class)


-- model


type alias Model =
    Page


init : ( Model, Cmd Msg )
init =
    ( NotFound
    , Cmd.none
    )


type Page
    = NotFound



-- update


type Msg
    = Navigate Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                NotFound ->
                    div []
                        [ h1 []
                            [ text "Page not found"
                            ]
                        ]
    in
        div []
            [ viewHeader model
            , div [ class "section" ]
                [ page
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
