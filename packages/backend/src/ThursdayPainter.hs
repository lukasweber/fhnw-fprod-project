{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ThursdayPainter where

import Protolude
import qualified Network.WebSockets as WS
import qualified WebSocketServer

main :: IO ()
main = do
  st <- newMVar WebSocketServer.newServerState
  WS.runServer "127.0.0.1" 3000 $ WebSocketServer.application st
