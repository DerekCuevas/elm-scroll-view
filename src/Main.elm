module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time
import Ease
import ScrollView


scrollViewConfig : ScrollView.Config
scrollViewConfig =
    { id = "items-scroll-view"
    , ease = Ease.inOutSine
    , duration = Time.second / 2
    }



---- MODEL ----


type alias Model =
    { items : List Int
    , scrollView : ScrollView.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( scrollView, scrollViewCmd ) =
            ScrollView.init scrollViewConfig
    in
        ( { items = List.range 0 20
          , scrollView = scrollView
          }
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
                    ScrollView.update scrollViewConfig svmsg model.scrollView
            in
                ( { model | scrollView = scrollView }
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
    div [ class "container" ]
        [ h3 [] [ text "Test app:" ]
        , viewScrollView model
        ]


viewScrollView : Model -> Html Msg
viewScrollView model =
    ScrollView.view scrollViewConfig
        model.scrollView
        { items = List.map viewItem model.items
        , toMsg = ScrollViewMsg
        }


viewItem : Int -> Html Msg
viewItem index =
    div [ class "item" ] [ text (toString index) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
