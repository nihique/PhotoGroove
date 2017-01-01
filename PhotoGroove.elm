module PhotoGroove exposing (..)

import Array exposing (Array)
import Http
import Html exposing (Html, program, button, div, h1, img, input, label, text)
import Html.Attributes exposing (class, classList, id, name, selected, src, type_)
import Html.Events exposing (onClick)
import Html.Events exposing (onClick)
import Random


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, initialCommand )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initialCommand : Cmd Msg
initialCommand =
    urlPhotos
        |> Http.getString
        |> Http.send LoadPhotos


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
    = LoadPhotos (Result Http.Error String)
    | SelectByUrl (Maybe String)
    | SelectByIndex Int
    | SurpriseMe
    | SelectSize ThumbnailSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPhotos (Ok photosString) ->
            let
                photos =
                    photosString
                        |> String.split ","
                        |> List.map Photo

                selectedUrl =
                    photos
                        |> List.head
                        |> Maybe.map .url
            in
                ( { model
                    | photos = photos
                    , selectedUrl = selectedUrl
                  }
                , Cmd.none
                )

        LoadPhotos (Err _) ->
            ( model, Cmd.none )

        SelectByUrl url ->
            ( { model | selectedUrl = url }, Cmd.none )

        SelectByIndex index ->
            let
                url =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                ( { model | selectedUrl = url }, Cmd.none )

        SurpriseMe ->
            let
                randomGenerator =
                    Random.int 0 <| List.length model.photos - 1

                command =
                    Random.generate SelectByIndex randomGenerator
            in
                ( model, command )

        SelectSize size ->
            ( { model | chosenSize = size }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , viewSurpriseMeButton
        , viewThumbnailSizer
        , viewThumbnails model.selectedUrl model.chosenSize model.photos
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


viewThumbnails : Maybe String -> ThumbnailSize -> List Photo -> Html Msg
viewThumbnails selectedUrl chosenSize photos =
    div
        [ id "thumbnails"
        , class (thumbnailSizeToString chosenSize)
        ]
        (List.map
            (viewThumbnail selectedUrl)
            photos
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
                , src <| urlPrefix ++ "large/" ++ url
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


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


urlPhotos : String
urlPhotos =
    "http://elm-in-action.com/photos/list"
