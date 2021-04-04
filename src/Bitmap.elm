module Bitmap exposing (Bitmap, decoder, toDict)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Bytes
import Dict exposing (Dict)


type alias Metadata =
    { header : FileHeader
    , dibHeader : DibHeader
    }


type alias FileHeader =
    { field : HeaderField
    , filesizeInBytes : Int
    , reserved1 : Int
    , reserved2 : Int
    , pixelOffset : Int
    }


type alias DibHeader =
    { dibHeaderType : ( DibHeaderType, Int )
    , dimensions : ( Int, Int )
    , planes : Int
    , colorDepth : Int
    , compressionMethod : Int
    }


type HeaderField
    = BM


type DibHeaderType
    = Windows
    | OS2


decoder : Bytes.Decoder Bitmap
decoder =
    let
        {- https://en.wikipedia.org/wiki/BMP_file_format#File_structure -}
        fieldHeader : Bytes.Decoder HeaderField
        fieldHeader =
            Bytes.map2 (\a b -> String.fromList [ Char.fromCode a, Char.fromCode b ])
                Bytes.unsignedInt8
                Bytes.unsignedInt8
                |> Bytes.andThen
                    (\str ->
                        case str of
                            "BM" ->
                                Bytes.succeed BM

                            _ ->
                                Bytes.fail
                    )

        dibHeaderType : Bytes.Decoder ( DibHeaderType, Int )
        dibHeaderType =
            Bytes.unsignedInt32 LE
                |> Bytes.andThen
                    (\size_ ->
                        case size_ of
                            40 ->
                                Bytes.succeed ( Windows, size_ )

                            12 ->
                                Bytes.succeed ( OS2, size_ )

                            _ ->
                                Bytes.fail
                    )

        imageDimensions =
            Bytes.map2 Tuple.pair
                (Bytes.signedInt32 LE)
                (Bytes.signedInt32 LE)

        metadataDecoder =
            Bytes.map2 Metadata
                (Bytes.map5 FileHeader
                    fieldHeader
                    (Bytes.unsignedInt32 LE)
                    (Bytes.unsignedInt16 LE)
                    (Bytes.unsignedInt16 LE)
                    (Bytes.unsignedInt32 LE)
                )
                (Bytes.map5 DibHeader
                    dibHeaderType
                    imageDimensions
                    (Bytes.unsignedInt16 LE)
                    (Bytes.unsignedInt16 LE)
                    (Bytes.unsignedInt32 LE)
                )

        pixelDecoder : Bytes.Decoder Color
        pixelDecoder =
            Bytes.map3 (\b g r -> ( r, g, b ))
                Bytes.unsignedInt8
                Bytes.unsignedInt8
                Bytes.unsignedInt8

        fromMetadata metadata =
            let
                fileHeaderSize =
                    14

                ( _, dibHeaderSize ) =
                    metadata.dibHeader.dibHeaderType

                bytesToIgnore =
                    metadata.header.pixelOffset - dibHeaderSize - fileHeaderSize + 20

                ( width, height ) =
                    metadata.dibHeader.dimensions

                helper : ( Int, Bitmap_ ) -> Bytes.Decoder (Bytes.Step ( Int, Bitmap_ ) Bitmap_)
                helper ( pixels, bitmap ) =
                    if pixels >= width * height then
                        Bytes.succeed (Bytes.Done bitmap)

                    else
                        let
                            ( x, y ) =
                                ( modBy width pixels, pixels // width )
                        in
                        pixelDecoder
                            |> Bytes.map
                                (\pixel ->
                                    Dict.insert ( x, height - y - 1 ) pixel bitmap
                                        |> Tuple.pair (pixels + 1)
                                        |> Bytes.Loop
                                )
            in
            Bytes.bytes bytesToIgnore
                |> Bytes.andThen
                    (\_ ->
                        Bytes.loop ( 0, Dict.empty ) helper
                    )
    in
    metadataDecoder
        |> Bytes.andThen fromMetadata
        |> Bytes.map Bitmap


type Bitmap
    = Bitmap Bitmap_


type alias Color =
    ( Int, Int, Int )


type alias Bitmap_ =
    Dict ( Int, Int ) Color


toDict : Bitmap -> Dict ( Int, Int ) Color
toDict (Bitmap bmp) =
    bmp
