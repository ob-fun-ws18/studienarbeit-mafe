module Client (
      Msg(..)
    , parseMsg
    ) where

data Msg = Plain String
        | Nick String
    deriving (Show, Eq)

parseMsg :: String -> Msg
parseMsg str = Plain str
