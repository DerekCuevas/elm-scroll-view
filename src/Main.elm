module Main exposing (..)

import Html exposing (..)
import ScrollView


---- MODEL ----


type alias Model =
    { scrollView : ScrollView.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( scrollView, scrollViewCmd ) =
            ScrollView.init
    in
        ( { scrollView = scrollView }
        , Cmd.map ScrollViewMsg scrollViewCmd
        )



---- UPDATE ----


type Msg
    = ScrollViewMsg ScrollView.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollViewMsg svmsg ->
            let
                ( scrollView, scrollViewCmd ) =
                    ScrollView.update svmsg model.scrollView
            in
                ( { scrollView = scrollView }
                , Cmd.map ScrollViewMsg scrollViewCmd
                )



---- SUBSCRIPTONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    ScrollView.subscriptions model.scrollView
        |> Sub.map ScrollViewMsg



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ text "Test app:"
        , viewScrollView model.scrollView
        ]


viewScrollView : ScrollView.Model -> Html Msg
viewScrollView model =
    ScrollView.view model
        |> Html.map ScrollViewMsg



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
