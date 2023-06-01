{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer where

import Protolude
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- Client is a tuple of the client's name and the connection
type Client = (Text, WS.Connection)
type ServerState = [Client]

type Point = (Int, Int)
type Points = [Point]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- Broadcast a message to all registered clients
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- Main entry point for the socket server
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
      msg <- WS.receiveData conn
      clients <- readMVar state
      case msg of
        _ -> do
          let client = (msg, conn)

          -- The first message that we expect from a client is their name.
          -- After the first contact, we start listening for delta updates
          -- to the canvas.
          readMVar state >>= broadcast ("User connected: " `mappend` fst client)
          modifyMVar_ state $ \s -> return $ addClient client s
          talk client state `finally` do
              readMVar state >>= broadcast ("User disconnected: " `mappend` fst client)
              modifyMVar_ state $ \s -> return $ removeClient client s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast msg
