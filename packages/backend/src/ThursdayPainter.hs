{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ThursdayPainter where

import Protolude
import qualified Network.WebSockets as WS
import qualified Server
import qualified WebSocket

main :: IO ()
main = do
  st <- newMVar WebSocket.newServerState
  WS.runServer "127.0.0.1" 3000 $ Server.application st
