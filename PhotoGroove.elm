module PhotoGroove exposing (..)

import Array exposing (Array)
import Html exposing (Html, program, button, div, h1, img, input, label, text)
import Html.Attributes exposing (class, classList, id, name, selected, src, type_)
import Html.Events exposing (onClick)
import Html.Events exposing (onClick)
import Random


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
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


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SelectSize ThumbnailSize


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            ( { model | selectedUrl = url }
            , Cmd.none
            )

        SelectByIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }
            , Cmd.none
            )

        SurpriseMe ->
            ( model
            , Random.generate SelectByIndex randomGeneratorPhotoIndex
            )

        SelectSize size ->
            ( { model | chosenSize = size }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , viewSurpriseMeButton
        , viewThumbnailSizer
        , viewThumbnails model.selectedUrl model.chosenSize
        , viewPhoto model.selectedUrl
        ]


viewSurpriseMeButton : Html Msg
viewSurpriseMeButton =
    button
        [ onClick SurpriseMe ]
        [ text "Surprise Me!" ]


viewThumbnailSizer : Html Msg
viewThumbnailSizer =
    div
        [ id "choose-size" ]
        (List.map
            viewThumbnailSizerRadioButton
            [ Small, Medium, Large ]
        )


viewThumbnailSizerRadioButton : ThumbnailSize -> Html Msg
viewThumbnailSizerRadioButton thumbnailSize =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (SelectSize thumbnailSize)
            ]
            []
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
        , classList
            [ ( "selected", thumbnail.url == selectedUrl )
            ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewPhoto : String -> Html Msg
viewPhoto selectedUrl =
    img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


thumbnailSizeToString : ThumbnailSize -> String
thumbnailSizeToString thumbnailSize =
    case thumbnailSize of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


randomGeneratorPhotoIndex : Random.Generator Int
randomGeneratorPhotoIndex =
    Random.int 0 (Array.length photoArray - 1)
