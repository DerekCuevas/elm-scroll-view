port module ScrollView
    exposing
        ( Config
        , ViewConfig
        , Model
        , Msg
        , init
        , update
        , subscriptions
        , view
        , remeasure
        , isOverflowingLeft
        , isOverflowingRight
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
    { width : Float
    , height : Float
    }


initRect : Rect
initRect =
    { width = 0
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


scrollLeft : String -> Float -> Cmd Msg
scrollLeft scrollViewId scrollLeft =
    Dom.Scroll.toX scrollViewId scrollLeft
        |> Task.attempt ScrollResult



---- MODEL ----


type alias Config =
    { id : String
    , ease : Float -> Float
    , duration : Time
    }


type alias Model =
    { rect : Rect
    , scrollWidth : Float
    , scrollLeftAnimation : Animation
    , clock : Time
    }


init : Config -> ( Model, Cmd Msg )
init config =
    ( { rect = initRect
      , scrollWidth = 0
      , scrollLeftAnimation = Animation.static 0
      , clock = 0
      }
    , remeasure config.id
    )


isOverflowingLeft : Model -> Bool
isOverflowingLeft { scrollLeftAnimation } =
    Animation.getTo scrollLeftAnimation /= 0


isOverflowingRight : Model -> Bool
isOverflowingRight { rect, scrollWidth, scrollLeftAnimation } =
    rect.width + (Animation.getTo scrollLeftAnimation) < scrollWidth


animateScrollLeft : Config -> Model -> Animation
animateScrollLeft config { clock, rect, scrollWidth, scrollLeftAnimation } =
    let
        scrollLeftTo =
            Basics.min (scrollWidth - rect.width)
                (Animation.getTo scrollLeftAnimation + rect.width)
    in
        Animation.retarget clock scrollLeftTo scrollLeftAnimation
            |> Animation.ease config.ease
            |> Animation.duration config.duration


animateScrollRight : Config -> Model -> Animation
animateScrollRight config { clock, rect, scrollWidth, scrollLeftAnimation } =
    let
        scrollLeftTo =
            Basics.max 0 (Animation.getTo scrollLeftAnimation - rect.width)
    in
        Animation.retarget clock scrollLeftTo scrollLeftAnimation
            |> Animation.ease config.ease
            |> Animation.duration config.duration



---- UPDATE ----


type Msg
    = ScrollLeft
    | ScrollRight
    | ScrollResult (Result Dom.Error ())
    | SetBoundingClientRect { id : String, rect : Rect }
    | SetScrollWidth { id : String, scrollWidth : Float }
    | Resize Window.Size
    | Tick Time


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        ScrollLeft ->
            ( { model | scrollLeftAnimation = animateScrollLeft config model }
            , Cmd.none
            )

        ScrollRight ->
            ( { model | scrollLeftAnimation = animateScrollRight config model }
            , Cmd.none
            )

        ScrollResult _ ->
            ( model, Cmd.none )

        SetBoundingClientRect { id, rect } ->
            if id == config.id then
                ( { model | rect = rect }, Cmd.none )
            else
                ( model, Cmd.none )

        SetScrollWidth { id, scrollWidth } ->
            if id == config.id then
                ( { model | scrollWidth = scrollWidth }, Cmd.none )
            else
                ( model, Cmd.none )

        Resize _ ->
            ( model, remeasure config.id )

        Tick time ->
            let
                clock =
                    model.clock + time
            in
                ( { model | clock = clock }
                , Animation.animate clock model.scrollLeftAnimation
                    |> scrollLeft config.id
                )



---- SUBSCRIPTONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , setBoundingClientRect SetBoundingClientRect
        , setScrollWidth SetScrollWidth
        , if Animation.isDone model.clock model.scrollLeftAnimation then
            Sub.none
          else
            AnimationFrame.diffs Tick
        ]



---- VIEW ----


type alias ViewConfig msg =
    { items : List (Html msg)
    , toMsg : Msg -> msg
    }


view : Config -> Model -> ViewConfig msg -> Html msg
view config model viewConfig =
    div []
        [ div [ id config.id, class "scroll-view-items" ] viewConfig.items
        , button
            [ onClick (viewConfig.toMsg ScrollRight)
            , disabled (not (isOverflowingLeft model))
            ]
            [ text "<" ]
        , button
            [ onClick (viewConfig.toMsg ScrollLeft)
            , disabled (not (isOverflowingRight model))
            ]
            [ text ">" ]
        ]
