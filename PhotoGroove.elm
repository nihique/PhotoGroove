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
    , errorMessage : Maybe String
    , photos : List Photo
    , selectedUrl : Maybe String
    }


initialModel : Model
initialModel =
    { chosenSize = Medium
    , errorMessage = Nothing
    , photos = []
    , selectedUrl = Nothing
    }


type Msg
    = SelectByUrl (Maybe String)
    | SelectByIndex Int
    | SurpriseMe
    | SelectSize ThumbnailSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            ( { model | selectedUrl = url }
            , Cmd.none
            )

        SelectByIndex index ->
            ( { model | selectedUrl = getPhotoUrlByIndex index model.photos }
            , Cmd.none
            )

        SurpriseMe ->
            ( model
            , Random.generate SelectByIndex (photoIndexRandomGenerator model.photos)
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


viewThumbnails : Maybe String -> ThumbnailSize -> Html Msg
viewThumbnails selectedUrl chosenSize =
    div
        [ id "thumbnails"
        , class (thumbnailSizeToString chosenSize)
        ]
        (List.map
            (viewThumbnail selectedUrl)
            initialModel.photos
        )


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (urlPrefix ++ photo.url)
        , classList
            [ ( "selected", selectedUrl == Just photo.url )
            ]
        , onClick (SelectByUrl (Just photo.url))
        ]
        []


viewPhoto : Maybe String -> Html Msg
viewPhoto selectedUrl =
    case selectedUrl of
        Nothing ->
            text ""

        Just url ->
            img
                [ class "large"
                , src (urlPrefix ++ "large/" ++ url)
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


photoIndexRandomGenerator : List Photo -> Random.Generator Int
photoIndexRandomGenerator photos =
    Random.int 0 (List.length photos - 1)


getPhotoUrlByIndex : Int -> List Photo -> Maybe String
getPhotoUrlByIndex index photos =
    photos
        |> Array.fromList
        |> Array.get index
        |> Maybe.map .url
