module Client (
      Msg
    , parseMsg
    ) where

data Msg = Plain String
    deriving Show

parseMsg :: String -> Msg
parseMsg str = Plain str
