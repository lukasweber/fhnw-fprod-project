port module Main exposing (..)

import Browser
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (..)
import Color exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)
import List

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
    { isInit : Bool
    , points : List ( Float, Float )
    , isDrawing : Bool
    , username : String
    , memberList : List String
    , word : String
    , isDrawer : Bool
    , curentDrawer : String
    , guess : String
    , showVictoryScreen : Bool
    , winnerUsername : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { points = []
      , isDrawing = False
      , username = ""
      , isInit = True
      , isDrawer = False
      , memberList = []
      , word = ""
      , curentDrawer = ""
      , guess = ""
      , showVictoryScreen = False
      , winnerUsername = ""
      }
    , Cmd.none
    )

-- UPDATE
type Msg
    = StartDrawing
    | StopDrawing
    | MouseMove ( Float, Float )
    | ReceiveWS String
    | SendWS String
    | UpdateUsername String
    | JoinGame
    | GuessWord
    | UpdateGuessWord String
    | PlayAgain

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDrawing ->
            ( { model | isDrawing = True }, Cmd.none )
        StopDrawing ->
            ( { model | isDrawing = False }, Cmd.none )
        MouseMove ( x, y ) ->
            if model.isDrawing then
                ( { model | points = ( x, y ) :: model.points }, sendMessage ("D:" ++ toString x ++ "," ++ toString y) )
            else
                ( model, Cmd.none )
        ReceiveWS m -> handleMessage m model
        SendWS m ->
            ( model, sendMessage m )
        UpdateUsername m ->
            ( { model | username = m }, Cmd.none )
        JoinGame ->
            ( { model | isInit = False }, sendMessage model.username )
        UpdateGuessWord m ->
            ( { model | guess = m }, Cmd.none )
        GuessWord ->
            ( model, sendMessage ("G:" ++ model.guess) )
        PlayAgain ->
            ( { model | showVictoryScreen = False }, Cmd.none )

handleMessage : String -> Model -> ( Model, Cmd Msg )
handleMessage m model = 
        if String.startsWith "D:" m then
              ( { model | points = convertToPoint (String.dropLeft 2 m) :: model.points }, Cmd.none )
          else if String.startsWith "C:" m then
              ( { model | word = String.dropLeft 2 m }, Cmd.none )
          else if String.startsWith "E:" m then
              ( { model | isDrawer = model.username == String.dropLeft 2 m, curentDrawer = String.dropLeft 2 m, points = [] }, Cmd.none )
          else if String.startsWith "U:" m then
              ( { model | memberList = String.split "," (String.dropLeft 2 m) }, Cmd.none )
          else if String.startsWith "J:" m then
              ( { model | memberList = model.memberList ++ [ String.dropLeft 2 m ] }, Cmd.none )
          else if String.startsWith "L:" m then
              ( { model | memberList = List.filter (\x -> x /= String.dropLeft 2 m) model.memberList }, Cmd.none )
          else if String.startsWith "V:" m then
              ( { model | showVictoryScreen = True, winnerUsername = String.dropLeft 2 m }, Cmd.none )
          else if String.startsWith "R:" m then
              ( { model | points = [] }, Cmd.none )
          else
              ( model, Cmd.none )

convertToPoint : String -> ( Float, Float )
convertToPoint s =
    case String.split "," s of
        [ x, y ] ->
            ( convertStringToFloat x, convertStringToFloat y )
        _ ->
            ( 0, 0 )


convertStringToFloat : String -> Float
convertStringToFloat s =
    case String.toFloat s of
        Just f ->
            f
        Nothing ->
            0


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
        [ h1 [ class "logo" ] [ text "Thursday Painter" ]
        , if model.isInit then
            viewInitScreen model
          else if model.showVictoryScreen then
            viewVictoryScreen model.winnerUsername
          else
            viewDrawScreen model
        ]

viewDrawScreen : Model -> Html Msg
viewDrawScreen model =
    div [ class "container" ]
        [ viewDrawCanvas model
        , viewControls model
        ]

viewDrawCanvas : Model -> Html Msg
viewDrawCanvas model =
    div [ style "position" "relative", class "canvas" ]
          (div []
            (
              (if not model.isDrawer && not (model.curentDrawer == "") then [ viewCurrentDrawerBadge model.curentDrawer ] else []) ++
              [ Canvas.toHtml ( 500, 500 )
                ([ class "shadow-box"
                 , onMouseDown StartDrawing
                 , onMouseUp StopDrawing
                 ] ++ (if model.isDrawer then [ onMouseMove ] else [])
                )
                [ shapes [ fill Color.red ] (List.map (\( x, y ) -> rect ( x, y ) 4 4) model.points) ]
            ])
            :: (if not model.isDrawer then
                    [ viewGuessInput model ]
                else
                    []
                )
        )

viewInitScreen : Model -> Html Msg
viewInitScreen model =
    div [ class "container" ]
        [ div [ class "shadow-box colored-box init-screen" ]
            [ h1 [] [ text "Hi!" ]
            , p [] [ text "Please enter your name to start." ]
            , input [ type_ "text", placeholder "Enter your name", onInput UpdateUsername ] []
            , button [ onClick JoinGame, class "btn", disabled (model.username == "") ] [ text "Start" ]
            ]
        ]

viewVictoryScreen : String -> Html Msg
viewVictoryScreen winnerUsername =
    div [ class "container" ]
        [ div [ class "shadow-box colored-box init-screen", style "text-align" "center" ]
            [ h1 []
                [ text (winnerUsername ++ " won!")
                , br [] []
                , button [ onClick PlayAgain, class "btn" ] [ text "Play again" ]
                ]
            ]
        ]

viewControls : Model -> Html Msg
viewControls model =
    div [ class "controls-container" ]
        ([ label [] [ text "Members" ]
        , viewMemberList model
        , label [] [ text "Role" ]
        , viewRoleDisplay model
        ] ++ (
          if model.isDrawer then
            [ label [] [ text "Word" ]
              , span [ class "word-display" ] [ text model.word ]
            ] else []
        ))

viewRoleDisplay : Model -> Html Msg
viewRoleDisplay model =
    div [ class "role-display" ]
      [ span
          [ class (if model.isDrawer then "active" else "") ]
          [ text "Drawer" ]
      , span
          [ class (if not model.isDrawer then "active" else "" ) ]
          [ text "Guesser" ]
      ]


viewMemberList : Model -> Html Msg
viewMemberList model =
    div [ class "member-list" ]
      (span [] [ text (model.username ++ " (You)") ]
          :: List.map
              (\m ->
                  span
                      [ class (if m == model.curentDrawer then "active" else "") ]
                      [ text m ]
              )
              model.memberList
      )

viewCurrentDrawerBadge : String -> Html Msg
viewCurrentDrawerBadge currentDrawer =
    div
        [ class "drawer-badge" ]
        [ span [ class "drawer-name" ] [ text currentDrawer ]
        , text " is drawing"
        ]

viewGuessInput : Model -> Html Msg
viewGuessInput model =
    div [ class "guess-input-container" ]
        [ input [ type_ "text", placeholder "Enter your guess", onInput UpdateGuessWord, style "margin-right" "10px" ] []
        , button [ onClick GuessWord, class "btn", disabled (model.username == "") ] [ text "Guess" ]
        ]

onMouseMove : Attribute Msg
onMouseMove =
    Html.Events.on "mousemove"
        (Decode.map2 (\x -> \y -> MouseMove ( x, y ))
            (field "offsetX" float)
            (field "offsetY" float)
        )
