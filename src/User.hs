module User (
      User(..)
    ) where

data User = User String
        | NoUser
    deriving Show
