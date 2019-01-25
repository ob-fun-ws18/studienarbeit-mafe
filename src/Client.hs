{-|
Module: Client
Description: Message parser

This module contains functions to parse and build Messages.
-}
module Client (
      Msg(..)
    , parseMsg
    , buildMsg
    , parseCommand
    ) where

import Text.Regex.PCRE
import Data.List.Split
import User

-- |Messages that can be received from the client
data Msg = Plain String
        | Nick String                   -- ^Nickname message with the new nick
        | User String String            -- ^User message with username and full name
        | PrivMsg String String         -- ^Private message with receiver and content
        | Join String                   -- ^Join message with the channel
        | JoinMulti [String]            -- ^Join message with multiple channels
        | Part String String            -- ^Part message with the channel name and message
        | Ping String                   -- ^Ping message
        | Quit String                   -- ^Quit with message
        | IsOn [String]                 -- ^Is on message with the nick names
    deriving (Show, Eq)

-- |This function builds the messages for the client.
buildMsg :: User        -- ^The sender of the message
         -> Msg         -- ^The message
         -> String      -- ^Message to send to the client
buildMsg user (PrivMsg receiver msg) = ":" ++ toString user ++ " PRIVMSG " ++ receiver ++ " :" ++ msg
buildMsg user (Join channel) = ":" ++ toString user ++ " JOIN " ++ channel
buildMsg user (Part channel msg) = ":" ++ toString user ++ " PART " ++ channel ++ " :" ++ msg
buildMsg user (Quit msg) = ":" ++ toString user ++ " QUIT :" ++ msg
buildMsg (FullUser nick _ _) (IsOn users) = ":localhost 303 " ++ nick ++ " :" ++ unwords users

-- |This function parses the messages from the client.
parseMsg :: String      -- ^Message from the client
         -> Msg         -- ^Parsed message
parseMsg str = parseCommand cmd after
        where (_, cmd, after) = str =~ "^[A-Z]+ " :: (String, String, String)

-- |This function parses the additional information from the message.
parseCommand :: String  -- ^Type of the message
             -> String  -- ^Remaining message
             -> Msg     -- ^Parsed message
parseCommand "NICK " user = Nick user
parseCommand "USER " after = User user $ tail name
        where (before, name, _) = after =~ ":.*$" :: (String, String, String)
              user = before =~ "^\\S+"
parseCommand "PRIVMSG " after = PrivMsg (head groups) msg
        where (_, _, msg, groups) = after =~ "^(\\S+)\\s:" :: (String, String, String, [String])
parseCommand "JOIN " channel = JoinMulti $ splitOn "," channel
parseCommand "PART " after
        | after =~ "^(\\S+)\\s:" = Part (head groups) msg
        | otherwise = Part after ""
        where (_, _, msg, groups) = after =~ "^(\\S+)\\s:" :: (String, String, String, [String])
parseCommand "PING " server = Ping server
parseCommand "QUIT " after = Quit msg
        where msg = tail after
parseCommand "ISON " after = IsOn $ words after
parseCommand cmd after = Plain (cmd ++ after)
