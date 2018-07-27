module Euler exposing (..)

import Html exposing (Html, div, button, text, h3)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on)
import Time exposing (Time, second)
import Maybe exposing (withDefault)
import Window exposing (Size, size)
import Svg exposing (svg, circle, line, polyline)
import Svg.Attributes exposing (width, height, stroke, x1, x2, y1, y2, cx, cy, r, points, fill)
import Task exposing (perform)
import Slider exposing (..)
import Mouse
import Json.Decode as Decode
import Hex


main =
    Html.program
        { init = init
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
    , wWidth : Int
    , wHeight : Int
    , history : List ( Time, Time, Particle )
    , drag : Maybe Drag
    }


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
    { start : Position
    , current : Position
    }


getX : Particle -> Position
getX p =
    withDefault 0 <| List.head <| .pos p


getV : Particle -> Velocity
getV p =
    withDefault 0 <| List.head <| .vel p


getX0 : Model -> Position
getX0 m =
    let
        scale x =
            3 - 6 * x / (toFloat m.wHeight)
    in
        case m.drag of
            Nothing ->
                getX m.part

            Just { start, current } ->
                getX m.part + scale current - scale start



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model (Particle [ x0 ] [ 0 ]) 0.5 0.5 0 Idle 0 0 [] Nothing, perform GetSize size )


x0 : Position
x0 =
    2.5



-- UPDATE


type Msg
    = Start
    | Stop
    | Tick Time
    | GetSize Size
    | SliderUpdate Float
    | DragStart Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | status = Running
                , t = 0
                , dt = model.dt0
                , drag = Nothing
              }
            , Cmd.none
            )

        Stop ->
            ( { model
                | status = Idle
                , part = Particle [ x0 ] [ 0 ]
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
                        , perform GetSize size
                        )

        GetSize s ->
            ( { model | wWidth = s.width, wHeight = s.height * 8 // 10 }, Cmd.none )

        SliderUpdate dt ->
            ( { model | dt0 = dt }, Cmd.none )

        DragStart { x, y } ->
            case model.status of
                Idle ->
                    ( { model | drag = Just (Drag (toFloat y) (toFloat y)) }, Cmd.none )

                Running ->
                    ( model, Cmd.none )

        DragAt { x, y } ->
            ( { model | drag = Maybe.map (\{ start } -> Drag start (toFloat y)) model.drag }
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
diffEq x v t dt =
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
    case model.drag of
        Nothing ->
            Time.every (model.dt * second) Tick

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Drag the ball up or down, pick a dt and click Start" ]
        , h3 [ style [ ( "color", gradient model.dt0 ) ] ]
            [ viewSlider
            , text ("dt = " ++ toString model.dt0)
            , button [ onClick Start ] [ text "Start" ]
            , button [ onClick Stop ] [ text "Stop" ]
            ]
        , svg
            [ width (toString model.wWidth)
            , height (toString model.wHeight)
            , stroke "black"
            ]
            ([ line
                [ x1 "0"
                , x2 (toString model.wWidth)
                , y1 (toString (model.wHeight // 2))
                , y2 (toString (model.wHeight // 2))
                ]
                []
             , line
                [ x1 (toString (model.wWidth // 20))
                , x2 (toString (model.wWidth // 20))
                , y1 "0"
                , y2 (toString model.wHeight)
                ]
                []
             , viewCircle model
             ]
                ++ (plotHistory model)
            )
        ]


viewSlider : Html Msg
viewSlider =
    props2view [ MinVal 0, MaxVal 1, Step 0.01, onChange SliderUpdate ]


scaleX : Int -> Position -> String
scaleX h x =
    toString (toFloat h / 2 * (1 - x / 3))


scaleT : Int -> Time -> String
scaleT w t =
    toString (toFloat w * (0.05 + t / 5))


viewCircle : Model -> Html Msg
viewCircle m =
    circle
        [ cy (scaleX m.wHeight (getX0 m))
        , cx (scaleT m.wWidth m.t)
        , r "10"
        , on "mousedown" (Decode.map DragStart Mouse.position)
        ]
        []


plotPath : Int -> Int -> ( Time, Time, Particle ) -> String
plotPath w h ( dt, tf, particle ) =
    let
        comb x ( t, s ) =
            ( t - dt, s ++ (scaleT w t) ++ "," ++ (scaleX h x) ++ " " )
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
