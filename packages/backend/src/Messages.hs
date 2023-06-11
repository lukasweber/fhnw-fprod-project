{-# LANGUAGE OverloadedStrings #-}

module Messages where
import Protolude

data MessageType = JoinGame | LeftGame | Draw | WordGuess | ElectedUser | ChooseWord | CurrentUsers | Victory | Reset deriving (Eq,Ord,Enum,Show)

getMessageTypeShort :: MessageType -> Text
getMessageTypeShort JoinGame = "J"
getMessageTypeShort LeftGame = "L"
getMessageTypeShort Draw = "D"
getMessageTypeShort ChooseWord = "C"
getMessageTypeShort WordGuess = "G"
getMessageTypeShort ElectedUser = "E"
getMessageTypeShort CurrentUsers = "U"
getMessageTypeShort Victory = "V"
getMessageTypeShort Reset = "R"

getMessageType :: Text -> MessageType
getMessageType msg
  | "J:" `isPrefixOf` Protolude.toS msg = JoinGame
  | "L:" `isPrefixOf` Protolude.toS msg = LeftGame
  | "D:" `isPrefixOf` Protolude.toS msg = Draw
  | "C:" `isPrefixOf` Protolude.toS msg = ChooseWord
  | "G:" `isPrefixOf` Protolude.toS msg = WordGuess
  | "E:" `isPrefixOf` Protolude.toS msg = ElectedUser
  | "U:" `isPrefixOf` Protolude.toS msg = CurrentUsers
  | "V:" `isPrefixOf` Protolude.toS msg = Victory
  | "R:" `isPrefixOf` Protolude.toS msg = Reset
  | otherwise = error "Invalid message type"
