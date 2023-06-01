{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ThursdayPainter where

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
import qualified WebSocketServer as WebSocketServer

main :: IO ()
main = do
  let port = 3000
  state <- newMVar WebSocketServer.newServerState
  WS.runServer "127.0.0.1" 3000 $ WebSocketServer.application state






