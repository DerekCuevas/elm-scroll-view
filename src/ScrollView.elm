port module ScrollView
    exposing
        ( Model
        , Msg
        , init
        , update
        , subscriptions
        , view
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



-- COMMANDS --


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
    }


init : String -> ( Model, Cmd Msg )
init id =
    ( { rect = initRect }
    , getBoundingClientRect { id = id }
    )



---- UPDATE ----


type Msg
    = ScrollLeft
    | ScrollRight
    | ScrollResult (Result Dom.Error ())
    | SetBoundingClientRect { id : String, rect : Rect }
    | Resize Window.Size


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model id =
    case msg of
        ScrollLeft ->
            ( model, scrollLeft id model.rect )

        ScrollRight ->
            ( model, scrollRight id model.rect )

        ScrollResult result ->
            ( model, Cmd.none )

        SetBoundingClientRect { id, rect } ->
            ( { model | rect = rect }
            , Cmd.none
            )

        Resize _ ->
            ( model, getBoundingClientRect { id = id } )



---- SUBSCRIPTONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , setBoundingClientRect SetBoundingClientRect
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
