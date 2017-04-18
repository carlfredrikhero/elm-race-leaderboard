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


authPages : List Page
authPages =
    [ RunnerPage ]


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

        loggedIn =
            flags.token /= Nothing

        ( updatedPage, cmd ) =
            authRedirect page loggedIn

        ( leaderBoardInitModel, leaderboardCmd ) =
            Leaderboard.init

        ( loginInitModel, loginCmd, token ) =
            Login.init

        ( runnerInitModel, runnerCmd ) =
            Runner.init

        initModel =
            { page = updatedPage
            , leaderBoard = leaderBoardInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = flags.token
            , loggedIn = loggedIn
            }

        cmds =
            Cmd.batch
                [ Cmd.map LeaderboardMsg leaderboardCmd
                , Cmd.map LoginMsg loginCmd
                , Cmd.map RunnerMsg runnerCmd
                , cmd
                ]
    in
        ( initModel, cmds )


authenticatePage : Page -> Bool -> Bool
authenticatePage page loggedIn =
    loggedIn || not (List.member page authPages)


authRedirect : Page -> Bool -> ( Page, Cmd Msg )
authRedirect page loggedIn =
    if authenticatePage page loggedIn then
        ( page, Cmd.none )
    else
        ( LoginPage, Navigation.modifyUrl <| pageToHash LoginPage )



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
            let
                ( updatedPage, cmd ) =
                    authRedirect page model.loggedIn
            in
                ( { model | page = updatedPage }, cmd )

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


addRunnerLinkView : Model -> Html Msg
addRunnerLinkView { loggedIn } =
    if loggedIn then
        a
            [ class "nav-item"
            , onClick (Navigate RunnerPage)
            ]
            [ text "Add runner" ]
    else
        text ""


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
                , addRunnerLinkView model
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
