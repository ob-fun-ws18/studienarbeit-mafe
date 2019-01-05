module Client (
      Msg(..)
    , parseMsg
    ) where

import Text.Regex.PCRE

data Msg = Plain String
        | Nick String
        | User String String
        | PrivMsg String String
        | Join String
        | Part String String
        | Ping String
    deriving (Show, Eq)

buildMsg :: User -> Msg -> String
buildMsg user (PrivMsg receiver msg) = (":" ++ toString user ++ " PRIVMSG " ++ receiver ++ " :" ++ msg)

parseMsg :: String -> Msg
parseMsg str = parseCommand cmd after
        where (_, cmd, after) = str =~ "^[A-Z]+ " :: (String, String, String)

parseCommand :: String -> String -> Msg
parseCommand "NICK " user = Nick user
parseCommand "USER " after = User user $ tail name
        where (before, name, _) = after =~ ":.*$" :: (String, String, String)
              user = before =~ "^\\S+"
parseCommand "PRIVMSG " after = PrivMsg (head groups) msg
        where (_, _, msg, groups) = after =~ "^(\\S+)\\s:" :: (String, String, String, [String])
parseCommand "JOIN " channel = Join channel
parseCommand "PART " after = Part (head groups) msg
        where (_, _, msg, groups) = after =~ "^(\\S+)\\s:" :: (String, String, String, [String])
parseCommand "PING " server = Ping server
parseCommand cmd after = Plain (cmd ++ after)
