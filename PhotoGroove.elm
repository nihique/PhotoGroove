module PhotoGroove exposing (..)

import Array exposing (Array)
import Html exposing (Html, beginnerProgram, div, h1, img, text)
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


type alias Message =
    { operation : String
    , data : String
    }


update : Message -> Model -> Model
update msg model =
    case msg.operation of
        "THUMBNAIL_SELECTED" ->
            { model | selectedUrl = msg.data }

        _ ->
            model


view : Model -> Html Message
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
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


urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Message
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
