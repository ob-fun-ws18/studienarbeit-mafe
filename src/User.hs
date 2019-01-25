{-|
Module: User
Description: User information

This module handels information about the user
-}
module User (
      User(..)
    , toString
    ) where

-- |Type for storing user information
data User = FullUser String String String   -- ^User with nickname, username, full name
        | NickUser String                   -- ^User with nickname
        | NoUser                            -- ^User without information
    deriving Show

-- |Converts the user information into the identifying string
toString :: User -> String
toString (FullUser nick user _) = nick ++ "!" ++ user ++ "@localhost"
toString _ = "no!full@user"
