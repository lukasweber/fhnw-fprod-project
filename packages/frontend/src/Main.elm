port module Main exposing (..)

import Browser
import Debug exposing (toString)
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
    isInit : Bool,
    points: List (Float, Float),
    isDrawing: Bool,
    username: String
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { points = [], isDrawing = False, username = "", isInit = True}, Cmd.none
  )


-- UPDATE

type Msg
    = StartDrawing
    | StopDrawing
    | MouseMove (Float, Float)
    | ReceiveWS String
    | SendWS String
    | UpdateUsername String
    | JoinGame

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    StartDrawing ->
      ({ model | isDrawing = True}, Cmd.none )

    StopDrawing ->
      ({ model | isDrawing = False }, Cmd.none )

    MouseMove (x, y) ->
      if model.isDrawing then
        ({ model | points = (x, y) :: model.points }, sendMessage (toString x ++ "," ++ toString y))
      else
        ( model, Cmd.none )
    ReceiveWS m ->
      ({model | points = convertToPoint m :: model.points}, Cmd.none)
    SendWS m ->
      (model, sendMessage m)
    UpdateUsername m ->
      ({model | username = m}, Cmd.none)
    JoinGame ->
      ({ model | isInit = False }, sendMessage model.username)
      

convertToPoint : String -> (Float, Float)
convertToPoint s =
  case String.split "," s of
    [x, y] ->
      (convertStringToFloat x, convertStringToFloat y)
    _ ->
      (0, 0)

convertStringToFloat : String -> Float
convertStringToFloat s =
  case String.toFloat s of
    Just f -> f
    Nothing -> 0

-- SUBSCRIPTIONS


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver ReceiveWS 

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [ class "logo"] [ text "Thursday Painter" ]
    ,
      if model.isInit then
        viewInitScreen model
      else
        viewDrawScreen model
    ]

viewDrawScreen : Model -> Html Msg
viewDrawScreen model = div [ class "container"] [
    viewDrawCanvas model 
    , viewControls model
  ]

viewDrawCanvas : Model -> Html Msg
viewDrawCanvas model = Canvas.toHtml (500, 500)
            [ class "shadow-box", 
            onMouseDown StartDrawing, 
            onMouseUp StopDrawing, 
            onMouseMove]
            [ shapes [ fill Color.red ] (List.map (\(x, y) -> rect (x, y) 4 4) model.points) ]

viewInitScreen : Model -> Html Msg
viewInitScreen model = div [ class "container"] [
    div [ class "shadow-box colored-box init-screen" ] [
      h1 [] [ text "Hi!" ]
      , p [] [ text "Please enter your name to start." ]
      , input [ type_ "text", placeholder "Enter your name", onInput UpdateUsername ] []
      , button [ onClick JoinGame, class "btn", disabled (model.username == "") ] [ text "Start" ] 
    ]
  ]

viewControls : Model -> Html Msg
viewControls model = div [ class "controls-container" ] [
        label [] [ text "Name" ]
        , span [ class "name-display", style "margin-bottom" "20px" ] [ text model.username ]
        , label [] [ text "Role" ]
        , div [ class "role-display" ] [
          span [ class "active "] [ text "Drawer" ]
          , span [] [ text "Guesser" ]
        ]
        , label [] [ text "Word" ]
        , span [ class "word-display" ] [ text "Cat" ]
       ]
      
onMouseMove : Attribute Msg
onMouseMove =
  Html.Events.on "mousemove" (Decode.map2 (\x -> \y -> MouseMove (x, y))
      (field "offsetX" float)
      (field "offsetY" float))
