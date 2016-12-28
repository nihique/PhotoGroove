module PhotoGroove exposing (..)

import Array exposing (Array)
import Html exposing (Html, beginnerProgram, button, div, h1, img, input, label, text)
import Html.Attributes exposing (class, classList, id, name, selected, src, type_)
import Html.Events exposing (onClick)
import Html.Events exposing (onClick)


main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String
    }


type alias Model =
    { chosenSize : ThumbnailSize
    , photos : List Photo
    , selectedUrl : String
    }


initialModel : Model
initialModel =
    { chosenSize = Medium
    , photos =
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
    Array.fromList initialModel.photos


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
        , viewSurpriseMeButton
        , viewThumbnailSizer
        , viewThumbnails model.selectedUrl model.chosenSize
        , viewPhoto model.selectedUrl
        ]


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewSurpriseMeButton : Html Msg
viewSurpriseMeButton =
    button
        [ onClick { operation = "SURPRISE_ME", data = "" } ]
        [ text "Surprise Me!" ]


viewThumbnailSizer : Html Msg
viewThumbnailSizer =
    div
        [ id "choose-size" ]
        ( List.map
            viewThumbnailSizerRadioButton
            [ Small, Medium, Large ]
        )


viewThumbnailSizerRadioButton : ThumbnailSize -> Html Msg
viewThumbnailSizerRadioButton thumbnailSize =
    label []
        [ input [ type_ "radio", name "size" ] []
        , text (thumbnailSizeToString thumbnailSize)
        ]


viewThumbnails : String -> ThumbnailSize -> Html Msg
viewThumbnails selectedUrl chosenSize =
    div
        [ id "thumbnails"
        , class (thumbnailSizeToString chosenSize)
        ]
        (List.map
            (viewThumbnail selectedUrl)
            initialModel.photos
        )


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


viewPhoto : String -> Html Msg
viewPhoto selectedUrl =
    img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []


thumbnailSizeToString : ThumbnailSize -> String
thumbnailSizeToString thumbnailSize =
    case thumbnailSize of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"
