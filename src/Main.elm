port module Main exposing (..)

import Html exposing (Html, div, nav, a, span, text, header, h1, p)
import Html.Attributes exposing (href, class)
import Html.Events exposing (onClick)
import Navigation exposing (program, Location)
import Leaderboard
import Login
import Runner


-- model


type alias Model =
    { page : Page
    , leaderBoard : Leaderboard.Model
    , login : Login.Model
    , runner : Runner.Model
    , token : Maybe String
    , loggedIn : Bool
    }


type Page
    = NotFound
    | LeaderboardPage
    | LoginPage
    | RunnerPage


type alias Flags =
    { token : Maybe String
    }


port saveToken : String -> Cmd msg


port removeToken : () -> Cmd msg


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        page =
            hashToPage location.hash

        ( leaderBoardInitModel, leaderboardCmd ) =
            Leaderboard.init

        ( loginInitModel, loginCmd, token ) =
            Login.init

        ( runnerInitModel, runnerCmd ) =
            Runner.init

        initModel =
            { page = page
            , leaderBoard = leaderBoardInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = flags.token
            , loggedIn = flags.token /= Nothing
            }

        cmds =
            Cmd.batch
                [ Cmd.map LeaderboardMsg leaderboardCmd
                , Cmd.map LoginMsg loginCmd
                , Cmd.map RunnerMsg runnerCmd
                ]
    in
        ( initModel, cmds )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | LeaderboardMsg Leaderboard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg
    | LogOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Navigation.newUrl <| pageToHash page )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        LeaderboardMsg msg ->
            let
                ( leaderboardModel, cmd ) =
                    Leaderboard.update msg model.leaderBoard
            in
                ( { model | leaderBoard = leaderboardModel }
                , Cmd.map LeaderboardMsg cmd
                )

        LoginMsg msg ->
            let
                ( loginModel, cmd, token ) =
                    Login.update msg model.login

                loggedIn =
                    token /= Nothing

                saveTokenCmd =
                    case token of
                        Just jwt ->
                            saveToken jwt

                        Nothing ->
                            Cmd.none
            in
                ( { model
                    | login = loginModel
                    , token = token
                    , loggedIn = loggedIn
                  }
                , Cmd.batch
                    [ Cmd.map LoginMsg cmd
                    , saveTokenCmd
                    ]
                )

        RunnerMsg msg ->
            let
                ( runnerModel, cmd ) =
                    Runner.update msg model.runner
            in
                ( { model | runner = runnerModel }
                , Cmd.map RunnerMsg cmd
                )

        LogOut ->
            ( { model
                | token = Nothing
                , loggedIn = False
              }
            , Cmd.batch
                [ removeToken ()
                , Navigation.newUrl (pageToHash LoginPage)
                ]
            )



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

                LeaderboardPage ->
                    Html.map LeaderboardMsg
                        (Leaderboard.view model.leaderBoard)

                LoginPage ->
                    Html.map LoginMsg
                        (Login.view model.login)

                RunnerPage ->
                    Html.map RunnerMsg
                        (Runner.view model.runner)
    in
        div []
            [ viewHeader model
            , div [ class "section" ]
                [ div [ class "container" ]
                    [ page
                    ]
                ]
            , p [] [ text (toString model) ]
            ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        logInOutButton =
            case model.loggedIn of
                True ->
                    a
                        [ class "nav-item"
                        , onClick LogOut
                        ]
                        [ text "Log out" ]

                False ->
                    a
                        [ class "nav-item"
                        , onClick (Navigate LoginPage)
                        ]
                        [ text "Login" ]
    in
        nav [ class "nav hero is-default" ]
            [ div [ class "container" ]
                [ a [ onClick (Navigate LeaderboardPage), class "nav-item logo" ] [ text "Race Results" ]
                , a
                    [ class "nav-item"
                    , onClick (Navigate RunnerPage)
                    ]
                    [ text "Add runner" ]
                , logInOutButton
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        leaderBoardSub =
            Leaderboard.subscriptions model.leaderBoard

        loginSub =
            Login.subscriptions model.login

        runnerSub =
            Runner.subscriptions model.runner
    in
        Sub.batch
            [ Sub.map LeaderboardMsg leaderBoardSub
            , Sub.map LoginMsg loginSub
            , Sub.map RunnerMsg runnerSub
            ]


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "#/" ->
            LeaderboardPage

        "" ->
            LeaderboardPage

        "#login" ->
            LoginPage

        "#runner" ->
            RunnerPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        LeaderboardPage ->
            "#/"

        NotFound ->
            "#notfound"

        LoginPage ->
            "#login"

        RunnerPage ->
            "#runner"


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


main : Program Flags Model Msg
main =
    Navigation.programWithFlags locationToMsg
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
