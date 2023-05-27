module Main%20copy exposing (..)port module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown, onMouseOver, onMouseUp)
import Canvas exposing (..)
import Color exposing (..)
import Mouse exposing (..)

port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg


type alias Model =
    { canvas : Canvas.Model
    , isDrawing : Bool
    }


type Msg
    = CanvasMsg Canvas.Msg
    | StartDrawing
    | StopDrawing
    | UpdateMousePosition (Int, Int)


init : Model
init =
    { canvas = Canvas.init
    , isDrawing = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CanvasMsg (Canvas.subscriptions model.canvas)
        , Mouse.moves (\pos -> UpdateMousePosition (fst pos, snd pos))
        , Mouse.downs (\_ -> StartDrawing)
        , Mouse.ups (\_ -> StopDrawing)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        CanvasMsg canvasMsg ->
            { model | canvas = Canvas.update canvasMsg model.canvas }

        StartDrawing ->
            { model | isDrawing = True }

        StopDrawing ->
            { model | isDrawing = False }

        UpdateMousePosition pos ->
            if model.isDrawing then
                { model | canvas = Canvas.drawLine (lineStyle Red) pos model.canvas }
            else
                model


lineStyle : Color -> LineStyle
lineStyle color =
    { defaultLineStyle | color = color, width = 3 }


view : Model -> Html Msg
view model =
    div []
        [ canvas
            [ width "800"
            , height "600"
            , style "border" "1px solid black"
            , onMouseDown StartDrawing
            , onMouseOver (UpdateMousePosition << Tuple.pair Mouse.x Mouse.y)
            , onMouseUp StopDrawing
            ]
            []
        ]


main =
    Browser.sandbox { init = init, update = update, view = view, subscriptions = subscriptions }

-- port module Main exposing (..)

-- import Browser
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- import Json.Decode as D

-- import Canvas exposing (toHtml, shapes, rect)
-- import Canvas.Settings exposing (..)
-- import Color exposing (..)
-- import Html exposing (Html)
-- import Html.Attributes exposing (style)



-- -- MAIN


-- main : Program () Model Msg
-- main =
--   Browser.element
--     { init = init
--     , view = view
--     , update = update
--     , subscriptions = subscriptions
--     }




-- -- PORTS


-- port sendMessage : String -> Cmd msg
-- port messageReceiver : (String -> msg) -> Sub msg



-- -- MODEL


-- type alias Model =
--   { draft : String
--   , messages : List String
--   }


-- init : () -> ( Model, Cmd Msg )
-- init flags =
--   ( { draft = "", messages = [] }
--   , Cmd.none
--   )



-- -- UPDATE


-- type Msg
--   = DraftChanged String
--   | Send
--   | Recv String


-- -- Use the `sendMessage` port when someone presses ENTER or clicks
-- -- the "Send" button. Check out index.html to see the corresponding
-- -- JS where this is piped into a WebSocket.
-- --
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--   case msg of
--     DraftChanged draft ->
--       ( { model | draft = draft }
--       , Cmd.none
--       )

--     Send ->
--       ( { model | draft = "" }
--       , sendMessage model.draft
--       )

--     Recv message ->
--       ( { model | messages = model.messages ++ [message] }
--       , Cmd.none
--       )



-- -- SUBSCRIPTIONS


-- -- Subscribe to the `messageReceiver` port to hear about messages coming in
-- -- from JS. Check out the index.html file to see how this is hooked up to a
-- -- WebSocket.
-- --
-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--   messageReceiver Recv



-- -- VIEW


-- view model =
--   div []
--     [ h1 [] [ text "Thursday Painter" ]
--     , ul []
--         (List.map (\msg -> li [] [ text msg ]) model.messages)
--     , input
--         [ type_ "text"
--         , placeholder "Draft"
--         , onInput DraftChanged
--         , on "keydown" (ifIsEnter Send)
--         , value model.draft
--         ]
--         []
--     , button [ onClick Send ] [ text "Send" ]
--     , Canvas.toHtml (500, 500)
--             [ style "border" "1px solid black" ]
--             [ shapes [ fill Color.red ] [ rect (0, 0) 10 10 ]
--             ]
--     ]



-- -- DETECT ENTER


-- ifIsEnter : msg -> D.Decoder msg
-- ifIsEnter msg =
--   D.field "key" D.string
--     |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")
