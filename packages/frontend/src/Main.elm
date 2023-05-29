port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)

import Canvas exposing (toHtml, shapes, rect)
import Canvas.Settings exposing (..)
import Color exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)

-- PORTS
port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg


-- MAIN
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Model =
  { 
    points: List (Float, Float),
    isDrawing: Bool
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { points = [], isDrawing = False }, Cmd.none
  )



-- UPDATE

type Msg
    = StartDrawing
    | StopDrawing
    | MouseMove (Float, Float)
    | ReceiveWS String
    | SendWS String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    StartDrawing ->
      ({ model | isDrawing = True}, Cmd.none )

    StopDrawing ->
      ({ model | isDrawing = False }, Cmd.none )

    MouseMove (x, y) ->
      if model.isDrawing then
        ({ model | points = (x, y) :: model.points }, Cmd.none )
      else
        ( model, Cmd.none )
    ReceiveWS m ->
      (model, Cmd.none)
    SendWS m ->
      (model, sendMessage m)

-- SUBSCRIPTIONS


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver ReceiveWS 

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Thursday Painter" ]
    , button [ onClick (SendWS "Button pressed") ] [ text "Send" ]
    , Canvas.toHtml (500, 500)
            [ style "border" "1px solid black", style "display" "block", style "margin" "0 auto", 
            onMouseDown StartDrawing, 
            onMouseUp StopDrawing, 
            onMouseMove]
            [ shapes [ fill Color.red ] (List.map (\(x, y) -> rect (x, y) 4 4) model.points) ]
    ]
onMouseMove : Attribute Msg
onMouseMove =
  Html.Events.on "mousemove" (Decode.map2 (\x -> \y -> MouseMove (x, y))
      (field "offsetX" float)
      (field "offsetY" float))