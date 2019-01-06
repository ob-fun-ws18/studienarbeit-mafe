module Client (
      Msg(..)
    , parseMsg
    , buildMsg
    ) where

import Text.Regex.PCRE
import User

data Msg = Plain String
        | Nick String
        | User String String
        | PrivMsg String String
        | Join String
        | Part String String
        | Ping String
        | IsOn [String]
    deriving (Show, Eq)

buildMsg :: User -> Msg -> String
buildMsg user (PrivMsg receiver msg) = ":" ++ toString user ++ " PRIVMSG " ++ receiver ++ " :" ++ msg
buildMsg user (Join channel) = ":" ++ toString user ++ " JOIN " ++ channel
buildMsg user (Part channel msg) = ":" ++ toString user ++ " PART " ++ channel ++ " :" ++ msg
buildMsg (FullUser nick _ _) (IsOn users) = ":localhost 303 " ++ nick ++ " :" ++ unwords users

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
parseCommand "PART " after
        | after =~ "^(\\S+)\\s:" = Part (head groups) msg
        | otherwise = Part after ""
        where (_, _, msg, groups) = after =~ "^(\\S+)\\s:" :: (String, String, String, [String])
parseCommand "PING " server = Ping server
parseCommand "ISON " after = IsOn $ words after
parseCommand cmd after = Plain (cmd ++ after)
