{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer where

import Protolude
import Control.Monad.Random
import System.Random()
import qualified Network.WebSockets as WS
import qualified Data.Text as T


type Client = (Text, WS.Connection)
data ServerState = ServerState
  { clients :: [Client]
  , currentWord :: Text
  , drawer :: Client
  }

type Point = (Int, Int)
type Points = [Point]
data MessageType = JoinGame | LeftGame | Draw | WordGuess | ElectedUser | ChooseWord | CurrentUsers | Victory deriving (Eq,Ord,Enum,Show)

possibleWords :: [Text]
possibleWords = ["apple", "banana", "orange", "pear", "grape", "pineapple", "strawberry", "blueberry", "raspberry", "blackberry", "mango", "watermelon", "melon", "cherry", "peach", "plum", "kiwi", "lemon", "lime", "coconut", "papaya", "apricot", "avocado", "fig", "grapefruit", "guava", "lychee", "nectarine", "olive", "pomegranate", "tangerine", "tomato", "cantaloupe", "dragonfruit", "durian", "jackfruit", "kumquat", "mangosteen", "persimmon", "quince", "rhubarb", "starfruit", "ugli fruit", "breadfruit", "carambola", "cherimoya", "custard apple", "date", "elderberry", "goji berry", "gooseberry", "honeydew", "loquat", "mulberry", "passion fruit", "plantain", "pomelo", "prickly pear", "quandong", "salak", "soursop", "tamarind", "ugni", "yuzu", "zucchini"]

chooseWord :: (MonadRandom m) => m Text
chooseWord = do
  let n = length possibleWords
  i <- getRandomR (0, n - 1)
  case drop i possibleWords of
    (x:_) -> return x
    []    -> return ""

electDrawer :: (MonadRandom m) => ServerState -> m Client
electDrawer st = do
  let n = length (clients st)
  i <- getRandomR (0, n - 1)
  case drop i (clients st) of
    (x:_) -> return x
    []    -> return ("", undefined)

newRound :: ServerState -> IO ServerState
newRound st = do
  word <- chooseWord
  electedUser <- electDrawer st
  broadcast (createMessage ElectedUser (fst electedUser)) st
  WS.sendTextData (snd electedUser) (createMessage ChooseWord word)
  return st { currentWord = word }

newServerState :: ServerState
newServerState = ServerState [] "" ("", undefined)

numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client = elem (fst client) . map fst . clients

addClient :: Client -> ServerState -> ServerState
addClient client st = st { clients = client : clients st }

removeClient :: Client -> ServerState -> ServerState
removeClient client st = st { clients = filter ((/= fst client) . fst) (clients st) }

-- Broadcast a message to all registered clients
broadcast :: Text -> ServerState -> IO ()
broadcast message st = do
  putStrLn message
  forM_ (clients st) $ \(_, conn) -> WS.sendTextData conn message

-- Main entry point for the socket server
application :: MVar ServerState -> WS.ServerApp
application st pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
      msg <- WS.receiveData conn
      let client = (msg, conn)

      -- The first message that we expect from a client is their name.
      -- After the first contact, we start listening for delta updates
      -- to the canvas.
      readMVar st >>= broadcast (createMessage JoinGame (fst client))
      modifyMVar_ st $ \s -> return $ addClient client s

      modifyMVar_ st $ \s -> do
        let users = filter ((/= fst client) . fst) (clients s)
        let usernames = map fst users

        unless (null usernames) $ do
          WS.sendTextData conn (createMessage CurrentUsers (T.intercalate "," usernames))

        if numClients s > 1
          then newRound s
          else return s

      talk client st `finally` do
        readMVar st >>= broadcast (createMessage LeftGame (fst client))
        modifyMVar_ st $ \s -> return $ removeClient client s

talk :: Client -> MVar ServerState -> IO ()
talk (username, conn) st = forever $ do
    msg <- WS.receiveData conn

    -- when draw message, broadcast to all clients
    when ("D:" `isPrefixOf` Protolude.toS msg) $ do
      readMVar st >>= broadcast msg

    -- check if guess matches with current word
    when ("G:" `isPrefixOf` Protolude.toS msg) $
      modifyMVar_ st $ \s -> do
        putStrLn (createMessage WordGuess (currentWord s))
        if msg == createMessage WordGuess (currentWord s)
          then do
            putStrLn msg
            newRound s
          else return s

createMessage :: MessageType -> Text -> Text
createMessage messageType message = toS $ getMessageTypeShort messageType <> ":" <> message

getMessageTypeShort :: MessageType -> Text
getMessageTypeShort JoinGame = "J"
getMessageTypeShort LeftGame = "L"
getMessageTypeShort Draw = "D"
getMessageTypeShort ChooseWord = "C"
getMessageTypeShort WordGuess = "G"
getMessageTypeShort ElectedUser = "E"
getMessageTypeShort CurrentUsers = "U"
getMessageTypeShort Victory = "V"
