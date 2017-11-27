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
import Task exposing (Task)
import Time exposing (Time)
import Window
import Dom
import Dom.Scroll
import AnimationFrame
import Animation exposing (Animation)


-- DATA --


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


scrollToX : String -> Float -> Cmd Msg
scrollToX scrollViewId scrollLeft =
    Task.succeed scrollLeft
        |> Task.map2 (,) (Dom.Scroll.toX scrollViewId scrollLeft)
        |> Task.map Tuple.second
        |> Task.attempt ScrollResult


scrollLeft : String -> Rect -> Float -> Float -> Cmd Msg
scrollLeft scrollViewId rect scrollLeft scrollWidth =
    Basics.min (scrollWidth - rect.width) (scrollLeft + rect.width)
        |> scrollToX scrollViewId


scrollRight : String -> Rect -> Float -> Cmd Msg
scrollRight scrollViewId rect scrollLeft =
    Basics.max 0 (scrollLeft - rect.width)
        |> scrollToX scrollViewId



---- MODEL ----


{-| FIXME: scrollLeft = animation
-}
type alias Model =
    { rect : Rect
    , scrollLeft : Float
    , scrollWidth : Float
    , scrollLeftAnimation : Animation
    , clock : Time
    }


init : String -> ( Model, Cmd Msg )
init scrollViewId =
    ( { rect = initRect
      , scrollLeft = 0
      , scrollWidth = 0
      , scrollLeftAnimation = Animation.static 0
      , clock = 0
      }
    , remeasure scrollViewId
    )


isOverflowingLeft : Model -> Bool
isOverflowingLeft model =
    model.scrollLeft /= 0


isOverflowingRight : Model -> Bool
isOverflowingRight { rect, scrollLeft, scrollWidth } =
    (rect.width + scrollLeft) < scrollWidth



---- UPDATE ----


type Msg
    = ScrollLeft
    | ScrollRight
    | ScrollResult (Result Dom.Error Float)
    | SetBoundingClientRect { id : String, rect : Rect }
    | SetScrollWidth { id : String, scrollWidth : Float }
    | Resize Window.Size
    | Tick Time


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model scrollViewId =
    case msg of
        ScrollLeft ->
            ( model, scrollLeft scrollViewId model.rect model.scrollLeft model.scrollWidth )

        ScrollRight ->
            ( model, scrollRight scrollViewId model.rect model.scrollLeft )

        ScrollResult (Ok scrollLeft) ->
            ( { model | scrollLeft = scrollLeft }, Cmd.none )

        ScrollResult (Err _) ->
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

        Tick time ->
            ( { model | clock = model.clock + time }, Cmd.none )



---- SUBSCRIPTONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
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
        , button
            [ onClick (config.toMsg ScrollRight)
            , disabled (not (isOverflowingLeft model))
            ]
            [ text "<" ]
        , button
            [ onClick (config.toMsg ScrollLeft)
            , disabled (not (isOverflowingRight model))
            ]
            [ text ">" ]
        ]
