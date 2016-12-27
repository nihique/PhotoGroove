module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


update msg model =
    case msg.operation of
        "THUMBNAIL_SELECTED" ->
            { model | selectedUrl = msg.data }

        _ ->
            model


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
