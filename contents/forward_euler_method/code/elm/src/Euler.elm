module Euler exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Events
import Hex
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder)
import Maybe
import SingleSlider as Slider
import Svg exposing (circle, line, polyline, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, width, x1, x2, y1, y2)
import Task
import Time exposing (Posix)


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { part : Particle
    , dt : Time
    , dt0 : Time
    , t : Time
    , status : Status
    , wWidth : Float
    , wHeight : Float
    , history : List ( Time, Time, Particle )
    , drag : Maybe Drag
    , slider : Slider.Model
    }


x0 : Position
x0 =
    2.5


init : ( Model, Cmd Msg )
init =
    ( { part = Particle [ x0 ] [ 0 ]
      , dt = 0.25
      , dt0 = 0.25
      , t = 0
      , status = Idle
      , wWidth = 0
      , wHeight = 0
      , history = []
      , drag = Nothing
      , slider =
            { min = 0
            , max = 1
            , step = 0.01
            , value = 0.25
            , minFormatter = \_ -> ""
            , maxFormatter = \_ -> ""
            , currentValueFormatter = \_ _ -> ""
            , disabled = False
            }
      }
    , Task.perform GetViewPort Browser.Dom.getViewport
    )


type alias Time =
    Float


type alias Position =
    Float


type alias Velocity =
    Float


type alias Particle =
    { pos : List Position, vel : List Velocity }


type Status
    = Idle
    | Running


type alias Drag =
    { start : Float
    , current : Float
    }


getX : Particle -> Position
getX p =
    Maybe.withDefault 0 <| List.head <| .pos p


getV : Particle -> Velocity
getV p =
    Maybe.withDefault 0 <| List.head <| .vel p


getX0 : Model -> Position
getX0 m =
    let
        scale x =
            3 - 6 * x / m.wHeight
    in
    case m.drag of
        Nothing ->
            getX m.part

        Just { start, current } ->
            getX m.part + scale current - scale start


resetParticle : Particle -> Particle
resetParticle { pos, vel } =
    case ( List.reverse pos, List.reverse vel ) of
        ( x :: _, v :: _ ) ->
            Particle [ x ] [ v ]

        _ ->
            Particle [ x0 ] [ 0 ]



-- UPDATE


type Msg
    = Start
    | Stop
    | Tick Posix
    | GetViewPort Viewport
    | SliderUpdate Float
    | SliderMsg Slider.Msg
    | DragStart Float
    | DragAt Float
    | DragEnd Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | status = Running
                , t = 0
                , dt = model.dt0
                , drag = Nothing
                , part = resetParticle model.part
              }
            , Cmd.none
            )

        Stop ->
            ( { model
                | status = Idle
                , part = resetParticle model.part
                , t = 0
              }
            , Cmd.none
            )

        Tick _ ->
            case model.status of
                Idle ->
                    ( model, Cmd.none )

                Running ->
                    if model.t > 5 + model.dt then
                        ( { model
                            | status = Idle
                            , part = Particle [ x0 ] [ 0 ]
                            , history = ( model.dt, model.t, model.part ) :: model.history
                            , t = 0
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | part = evolve model.part model.t model.dt
                            , t = model.t + model.dt
                          }
                        , Task.perform GetViewPort Browser.Dom.getViewport
                        )

        GetViewPort { viewport } ->
            ( { model | wWidth = viewport.width, wHeight = viewport.height * 8 / 10 }, Cmd.none )

        SliderUpdate dt ->
            ( { model | dt0 = dt }, Cmd.none )

        SliderMsg sliderMsg ->
            let
                ( newSlider, cmd, updateResults ) =
                    Slider.update sliderMsg model.slider

                newModel =
                    { model | slider = newSlider, dt0 = newSlider.value }

                newCmd =
                    if updateResults then
                        Cmd.batch [ Cmd.map SliderMsg cmd, Cmd.none ]

                    else
                        Cmd.none
            in
            ( newModel, newCmd )

        DragStart y ->
            case model.status of
                Idle ->
                    ( { model | drag = Just (Drag y y) }, Cmd.none )

                Running ->
                    ( model, Cmd.none )

        DragAt y ->
            ( { model | drag = Maybe.map (\{ start } -> Drag start y) model.drag }
            , Cmd.none
            )

        DragEnd _ ->
            ( { model
                | drag = Nothing
                , part = Particle [ getX0 model ] [ k * getX0 model ]
              }
            , Cmd.none
            )


k : Float
k =
    -2


diffEq : Position -> Velocity -> Time -> Time -> ( Position, Velocity )
diffEq x _ _ dt =
    ( x + (k * x) * dt, k * (x + (k * x) * dt) )


evolve : Particle -> Time -> Time -> Particle
evolve p t dt =
    let
        ( x, v ) =
            diffEq (getX p) (getV p) t dt
    in
    { p | pos = x :: p.pos, vel = v :: p.vel }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    (Slider.subscriptions model.slider |> Sub.map SliderMsg)
        :: (case model.drag of
                Nothing ->
                    [ Time.every (model.dt * 1000) Tick ]

                Just _ ->
                    [ Events.onMouseMove (Decode.map DragAt decodeMouseHeight)
                    , Events.onMouseUp (Decode.map DragEnd decodeMouseHeight)
                    ]
           )
        |> Sub.batch


decodeMouseHeight : Decoder Float
decodeMouseHeight =
    Decode.field "pageY" Decode.float



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Drag the ball up or down, pick a dt and click Start" ]
        , h3 [ style "color" (gradient model.dt0) ]
            [ viewSlider model.slider
            , button [ onClick Start ] [ text "Start" ]
            , button [ onClick Stop ] [ text "Stop" ]
            , text ("dt = " ++ String.fromFloat model.dt0)
            ]
        , svg
            [ width (String.fromFloat model.wWidth)
            , height (String.fromFloat model.wHeight)
            , stroke "black"
            ]
            ([ line
                [ x1 "0"
                , x2 (String.fromFloat model.wWidth)
                , y1 (String.fromFloat (model.wHeight / 2))
                , y2 (String.fromFloat (model.wHeight / 2))
                ]
                []
             , line
                [ x1 (String.fromFloat (model.wWidth / 20))
                , x2 (String.fromFloat (model.wWidth / 20))
                , y1 "0"
                , y2 (String.fromFloat model.wHeight)
                ]
                []
             , viewCircle model
             ]
                ++ plotHistory model
            )
        ]


viewSlider : Slider.Model -> Html Msg
viewSlider slider =
    Slider.view slider |> Html.map SliderMsg


scaleX : Float -> Position -> String
scaleX h x =
    String.fromFloat (h / 2 * (1 - x / 3))


scaleT : Float -> Time -> String
scaleT w t =
    String.fromFloat (w * (0.05 + t / 5))


viewCircle : Model -> Html Msg
viewCircle m =
    circle
        [ cy (scaleX m.wHeight (getX0 m))
        , cx (scaleT m.wWidth m.t)
        , r "10"
        , on "mousedown" (Decode.map DragStart decodeMouseHeight)
        ]
        []


plotPath : Float -> Float -> ( Time, Time, Particle ) -> String
plotPath w h ( dt, tf, particle ) =
    let
        comb x ( t, s ) =
            ( t - dt, s ++ scaleT w t ++ "," ++ scaleX h x ++ " " )
    in
    Tuple.second <| List.foldl comb ( tf, "" ) particle.pos


plotHistory : Model -> List (Html Msg)
plotHistory m =
    let
        ( w, h ) =
            ( m.wWidth, m.wHeight )
    in
    List.map
        (\( dt, t, p ) ->
            polyline
                [ stroke "black"
                , fill "none"
                , stroke (gradient dt)
                , points (plotPath w h ( dt, t, p ))
                ]
                []
        )
        (( m.dt, m.t, m.part ) :: m.history)


gradient : Time -> String
gradient dt =
    let
        ( r, g, b ) =
            ( round (255 * dt), 0, round (255 * (1 - dt)) )

        col =
            Hex.toString (256 * (256 * r + g) + b)
    in
    if String.length col < 6 then
        "#" ++ String.repeat (6 - String.length col) "0" ++ col

    else
        "#" ++ col
