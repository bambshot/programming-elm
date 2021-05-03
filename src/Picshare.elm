module Picshare exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias Model =
    { url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }



-- こうも書ける
-- initialModel : Model
-- initialModel =
--     Model ("https://programming-elm.com/" ++ "1.jpg") "Surfing" False


initialModel : Model
initialModel =
    { url = "https://programming-elm.com/" ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Cowabunga, dude!" ]
    , newComment = ""
    }


viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]


viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        -- コメントないとき
        [] ->
            text ""

        -- コメントあるとき
        _ ->
            div [ class "comments" ]
                [ ul []
                    (List.map viewComment comments)
                ]


viewComments : Model -> Html Msg
viewComments model =
    div []
        [ viewCommentList model.comments
        , form [ class "new-comment", onSubmit SaveComment ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value model.newComment

                -- 日本語入力では onInput が正常に動作しないことがあるらしい
                -- その場合は、`npx elm install elm-community/html-extra` から
                -- モジュールをインポートして `onChange` に書き換えると良いとか
                , onInput UpdateComment
                ]
                []
            , button
                [ disabled (String.isEmpty model.newComment) ]
                [ text "Save" ]
            ]
        ]


viewLoveButton : Model -> Html Msg
viewLoveButton model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick ToggleLike
            ]
            []
        ]


viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton model
            , h2 [ class "caption" ] [ text model.caption ]
            , viewComments model
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
        ]


type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment


saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" ->
            model

        _ ->
            { model
              -- Array.prototype.push() 的な. php の要素追加に似てる気もする
                | comments = model.comments ++ [ comment ]
                , newComment = ""
            }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLike ->
            { model | liked = not model.liked }

        UpdateComment comment ->
            { model | newComment = comment }

        SaveComment ->
            saveNewComment model


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
