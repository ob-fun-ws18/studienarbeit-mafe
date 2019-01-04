module Client (
      Msg(..)
    , parseMsg
    ) where

import Text.Regex.PCRE

data Msg = Plain String
        | Nick String
    deriving (Show, Eq)

parseMsg :: String -> Msg
parseMsg str = parseCommand cmd after
    where (_, cmd, after) = str =~ "^[A-Z]+ " :: (String, String, String)

parseCommand :: String -> String -> Msg
parseCommand "NICK " user = Nick user
parseCommand cmd after = Plain (cmd ++ after)
