module Elm2D exposing
    ( view, viewScaled
    , Element, rectangle, sprite
    , viewFollowCamera
    )

{-|

@docs view, viewScaled

@docs Element, rectangle, sprite

-}

import Color exposing (Color)
import Elm2D.Spritesheet exposing (Sprite)
import Html exposing (Html)
import Html.Attributes as Attr
import Internals.Renderables.Rectangle as Rectangle
import Internals.Renderables.Sprite as Sprite
import Internals.Settings exposing (Settings)
import Internals.Sprite
import WebGL


view :
    { size : ( Float, Float )
    , background : Color
    }
    -> List Element
    -> Html msg
view options children =
    WebGL.toHtml
        [ Attr.width (floor (Tuple.first options.size))
        , Attr.height (floor (Tuple.second options.size))
        , Attr.style "background-color" (Color.toCssString options.background)
        ]
        -- reverse & map
        (List.foldl
            (\item list -> render { size = options.size } item :: list)
            []
            children
        )


viewScaled :
    { window : ( Float, Float )
    , size : ( Float, Float )
    , background : Color
    }
    -> List Element
    -> Html msg
viewScaled options children =
    let
        ( ww, wh ) =
            options.window

        ( sw, sh ) =
            options.size

        ( width, height ) =
            if wh / sh < ww / sw then
                ( wh * sw / sh, wh )

            else
                ( ww, ww / sw * sh )
    in
    WebGL.toHtml
        [ Attr.width (floor width)
        , Attr.height (floor height)
        , Attr.style "background-color" (Color.toCssString options.background)
        ]
        -- reverse & map
        (List.foldl
            (\item list -> render { size = options.size } item :: list)
            []
            children
        )


viewFollowCamera :
    { window : ( Float, Float )
    , size : ( Float, Float )
    , background : Color
    , centeredOn : ( Float, Float )
    }
    -> List Element
    -> Html msg
viewFollowCamera options children =
    let
        ( ww, wh ) =
            options.window

        ( sw, sh ) =
            options.size

        ( width, height ) =
            if wh / sh < ww / sw then
                ( wh * sw / sh, wh )

            else
                ( ww, ww / sw * sh )

        ( cx, cy ) =
            options.centeredOn

        translated ( x, y ) =
            ( x - cx + sw / 2, y - cy + sh / 2 )

        offsetBy el =
            case el of
                Rectangle_ options_ ->
                    Rectangle_ { options_ | position = translated options_.position }

                SpriteElement options_ ->
                    SpriteElement { options_ | position = translated options_.position }
    in
    WebGL.toHtml
        [ Attr.width (floor width)
        , Attr.height (floor height)
        , Attr.style "background-color" (Color.toCssString options.background)
        ]
        -- reverse & map
        (List.foldl
            (\item list -> render { size = options.size } (offsetBy item) :: list)
            []
            children
        )



-- ELEMENTS


type Element
    = Rectangle_ Rectangle.Options
    | SpriteElement Sprite.Options


rectangle :
    { size : ( Float, Float )
    , position : ( Float, Float )
    , color : Color
    }
    -> Element
rectangle =
    Rectangle_


sprite :
    { sprite : Sprite
    , size : ( Float, Float )
    , position : ( Float, Float )
    }
    -> Element
sprite options =
    SpriteElement
        { sprite = Internals.Sprite.unwrap options.sprite
        , size = options.size
        , position = options.position
        }


render : Settings -> Element -> WebGL.Entity
render settings item =
    case item of
        Rectangle_ options ->
            Rectangle.view settings options

        SpriteElement options ->
            Sprite.view settings options
