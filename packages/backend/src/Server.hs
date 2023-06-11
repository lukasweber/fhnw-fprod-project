{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Protolude
import Control.Monad.Random
import System.Random()
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Control.Monad (when)
import GHC.Base (undefined)
import Messages
import WebSocket

-- Main entry point for the socket server
application :: MVar ServerState -> WS.ServerApp
application st pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
      msg <- WS.receiveData conn
      let client = (msg, conn)

      -- notify other clients that a new client has joined
      readMVar st >>= broadcast (createMessage JoinGame (fst client))

      modifyMVar_ st $ \s -> do
        -- add client to state
        let newState = addClient client s

        -- broadcast current users to new client
        let users = filter ((/= fst client) . fst) (clients newState)
        let usernames = map fst users
        unless (null usernames) $ do
          WS.sendTextData conn (createMessage CurrentUsers (T.intercalate "," usernames))

        -- start the game if more than one client is connected
        if numClients newState > 1
          then newRound newState
          else return newState

      -- handle client messages in talk
      talk client st `finally` do
        -- remove client from state and notify others if talk ends
        readMVar st >>= broadcast (createMessage LeftGame (fst client))
        modifyMVar_ st $ \s -> return $ removeClient client s

-- handler loop that is running for each client
talk :: Client -> MVar ServerState -> IO ()
talk (username, conn) st = forever $ do
    msg <- WS.receiveData conn
    -- execute message handler and update state
    readMVar st >>= \s -> do
      newSt <- handleMessage msg username s
      modifyMVar_ st $ \_ -> return newSt

-------- Message Handlers --------

handleDrawMessage :: Text -> Text -> ServerState -> IO ()
handleDrawMessage msg username s = do
  Control.Monad.when (username == fst (drawer s)) $ broadcast msg s

handleResetMessage :: Text -> Text -> ServerState -> IO ()
handleResetMessage msg username s = do
  Control.Monad.when (username == fst (drawer s)) $ broadcast msg s

handleWordGuessMessage :: Text -> Text -> ServerState -> IO ServerState
handleWordGuessMessage msg username s =
  if username /= fst (drawer s) && msg == createMessage WordGuess (currentWord s)
    then do
      broadcast (createMessage Victory username) s
      newRound s
    else return s

handleMessage :: Text -> Text -> ServerState -> IO ServerState
handleMessage msg username st = do
  let messageType = getMessageType msg
  case messageType of
    Draw -> do
      handleDrawMessage msg username st
      return st
    WordGuess -> handleWordGuessMessage msg username st
    Reset -> do
      handleResetMessage msg username st
      return st
    _ -> return st

createMessage :: MessageType -> Text -> Text
createMessage messageType message = toS $ getMessageTypeShort messageType <> ":" <> message

-------- Game Logic --------

possibleWords :: [Text]
possibleWords = ["computer","printer","phone","building","house","table","chair","desk","lamp","pen","pencil","paper","notebook"]

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
    []    -> return ("", GHC.Base.undefined)

newRound :: ServerState -> IO ServerState
newRound st = do
  word <- chooseWord
  electedUser <- electDrawer st
  broadcast (createMessage Reset "") st
  broadcast (createMessage ElectedUser (fst electedUser)) st
  WS.sendTextData (snd electedUser) (createMessage ChooseWord word)
  return st { currentWord = word, drawer = electedUser }
