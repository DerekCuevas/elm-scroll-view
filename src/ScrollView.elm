port module ScrollView
    exposing
        ( Model
        , Msg
        , init
        , update
        , subscriptions
        , view
        , remeasure
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Window
import Dom
import Dom.Scroll


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


initRect : Rect
initRect =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    }



-- PORTS --


port getBoundingClientRect : { id : String } -> Cmd msg


port setBoundingClientRect : ({ id : String, rect : Rect } -> msg) -> Sub msg


port getScrollWidth : { id : String } -> Cmd msg


port setScrollWidth : ({ id : String, scrollWidth : Float } -> msg) -> Sub msg



-- COMMANDS --


remeasure : String -> Cmd Msg
remeasure scrollViewId =
    Cmd.batch
        [ getBoundingClientRect { id = scrollViewId }
        , getScrollWidth { id = scrollViewId }
        ]


scrollLeftBy : String -> (Float -> Float) -> Cmd Msg
scrollLeftBy id by =
    Dom.Scroll.x id
        |> Task.andThen (by >> Dom.Scroll.toX id)
        |> Task.attempt ScrollResult


scrollLeft : String -> Rect -> Cmd Msg
scrollLeft id rect =
    scrollLeftBy id (\left -> left + rect.width)


scrollRight : String -> Rect -> Cmd Msg
scrollRight id rect =
    scrollLeftBy id (\left -> left - rect.width)



---- MODEL ----


type alias Model =
    { rect : Rect
    , scrollLeft : Float
    , scrollWidth : Float
    }


init : String -> ( Model, Cmd Msg )
init scrollViewId =
    ( { rect = initRect
      , scrollLeft = 0
      , scrollWidth = 0
      }
    , remeasure scrollViewId
    )



---- UPDATE ----


type Msg
    = ScrollLeft
    | ScrollRight
    | ScrollResult (Result Dom.Error ())
    | SetBoundingClientRect { id : String, rect : Rect }
    | SetScrollWidth { id : String, scrollWidth : Float }
    | Resize Window.Size


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model scrollViewId =
    case msg of
        ScrollLeft ->
            ( model, scrollLeft scrollViewId model.rect )

        ScrollRight ->
            ( model, scrollRight scrollViewId model.rect )

        ScrollResult result ->
            ( model, Cmd.none )

        SetBoundingClientRect { id, rect } ->
            if id == scrollViewId then
                ( { model | rect = rect }, Cmd.none )
            else
                ( model, Cmd.none )

        SetScrollWidth { id, scrollWidth } ->
            if id == scrollViewId then
                ( { model | scrollWidth = scrollWidth }, Cmd.none )
            else
                ( model, Cmd.none )

        Resize _ ->
            ( model, remeasure scrollViewId )



---- SUBSCRIPTONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , setBoundingClientRect SetBoundingClientRect
        , setScrollWidth SetScrollWidth
        ]



---- VIEW ----


type alias Config msg =
    { id : String
    , items : List (Html msg)
    , toMsg : Msg -> msg
    }


view : Model -> Config msg -> Html msg
view model config =
    div []
        [ div [ id config.id, class "scroll-view-items" ] config.items
        , button [ onClick (config.toMsg ScrollRight) ] [ text "<" ]
        , button [ onClick (config.toMsg ScrollLeft) ] [ text ">" ]
        ]
