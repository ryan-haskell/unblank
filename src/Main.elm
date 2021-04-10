module Main exposing (main)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Effect
import Pages.Home_
import Shared
import Url exposing (Url)
import View


main : Program Shared.Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { shared : Shared.Model
    , page : Pages.Home_.Model
    }


init : Shared.Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( shared, sharedCmd ) =
            Shared.init flags

        ( page, cmd ) =
            Pages.Home_.init
    in
    ( Model shared page
    , Cmd.batch
        [ Cmd.map Shared sharedCmd
        , Cmd.map Page cmd
        ]
    )



-- UPDATE


type Msg
    = Shared Shared.Msg
    | Page Pages.Home_.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shared sharedMsg ->
            let
                ( shared, sharedCmd ) =
                    Shared.update sharedMsg model.shared
            in
            ( { model | shared = shared }
            , Cmd.map Shared sharedCmd
            )

        Page pageMsg ->
            let
                ( page, cmd ) =
                    Pages.Home_.update pageMsg model.page
            in
            ( { model | page = page }
            , Cmd.map Page cmd
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Pages.Home_.view model.shared model.page
        |> View.map Page
        |> View.toBrowserDocument



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Pages.Home_.subscriptions model.page |> Sub.map Page
        , Shared.subscriptions model.shared |> Sub.map Shared
        ]
