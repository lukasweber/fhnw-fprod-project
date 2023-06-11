{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module WebSocket where

import qualified GHC.Base
import qualified Network.WebSockets as WS
import Data.Text (Text)
import Prelude
import Control.Monad

type Client = (Text, WS.Connection)
data ServerState = ServerState
  { clients :: [Client]
  , currentWord :: Text
  , drawer :: Client
  }

newServerState :: ServerState
newServerState = ServerState [] "" ("", GHC.Base.undefined)

numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client = elem (fst client) . map fst . clients

addClient :: Client -> ServerState -> ServerState
addClient client st = st { clients = client : clients st }

removeClient :: Client -> ServerState -> ServerState
removeClient client st = st { clients = filter ((/= fst client) . fst) (clients st) }

broadcast :: Text -> ServerState -> IO ()
broadcast message st = do
    forM_ (clients st) $ \(_, conn) -> WS.sendTextData conn message
