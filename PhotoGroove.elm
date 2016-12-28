module PhotoGroove exposing (..)

import Array exposing (Array)
import Html exposing (Html, beginnerProgram, button, div, h1, img, text)
import Html.Attributes exposing (class, classList, id, src)
import Html.Events exposing (onClick)
import Html.Events exposing (onClick)


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Photo =
    { url : String
    }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }


model : Model
model =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


type alias Msg =
    { operation : String
    , data : String
    }


photoArray : Array Photo
photoArray =
    Array.fromList model.photos


update : Msg -> Model -> Model
update msg model =
    case msg.operation of
        "THUMBNAIL_SELECTED" ->
            { model | selectedUrl = msg.data }

        "SURPRISE_ME" ->
            { model | selectedUrl = "2.jpeg" }

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { operation = "SURPRISE_ME", data = "" } ]
            [ text "Surprise Me!" ]
        , div
            [ id "thumbnails" ]
            (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , onClick
            { operation = "THUMBNAIL_SELECTED"
            , data = thumbnail.url
            }
        , classList
            [ ( "selected", thumbnail.url == selectedUrl )
            ]
        ]
        []
