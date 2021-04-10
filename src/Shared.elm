module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Browser.Events
import Json.Decode as Json


type alias Flags =
    Json.Value


type alias Model =
    { window : { width : Float, height : Float }
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( flags
        |> Json.decodeValue decoder
        |> Result.withDefault
            { window = Dimensions 0 0
            }
    , Cmd.none
    )


decoder : Json.Decoder Model
decoder =
    Json.map Model
        (Json.field "window"
            (Json.map2 Dimensions
                (Json.field "width" Json.float)
                (Json.field "height" Json.float)
            )
        )


type Msg
    = WindowResized Dimensions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized window ->
            ( { model | window = window }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized (Dimensions (toFloat x) (toFloat y)))
