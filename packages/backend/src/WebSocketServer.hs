{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer where

import Protolude
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
data ServerState = ServerState
  { clients :: [Client]
  , currentWord :: Text
  }

type Point = (Int, Int)
type Points = [Point]
data MessageType = JoinGame | LeftGame | Draw | WordGuess | ElectedUser deriving (Eq,Ord,Enum,Show)

newServerState :: ServerState
newServerState = ServerState [] ""

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
        if Protolude.toS msg == createMessage WordGuess (currentWord s)
          then do
            broadcast (createMessage ElectedUser username) s
            return s { currentWord = "" }
          else return s

createMessage :: MessageType -> Text -> Text
createMessage messageType message = toS $ getMessageTypeShort messageType <> ":" <> message

getMessageTypeShort :: MessageType -> Text
getMessageTypeShort JoinGame = "J"
getMessageTypeShort LeftGame = "L"
getMessageTypeShort Draw = "D"
getMessageTypeShort WordGuess = "G"
getMessageTypeShort ElectedUser = "E"
