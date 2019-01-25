module User (
      User(..)
    , toString
    ) where

data User = FullUser String String String
        | NickUser String
        | NoUser
    deriving (Show, Eq)

toString :: User -> String
toString (FullUser nick user _) = nick ++ "!" ++ user ++ "@localhost"
toString _ = "no!full@user"
