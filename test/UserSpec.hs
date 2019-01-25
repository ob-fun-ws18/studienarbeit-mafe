{-# LANGUAGE ScopedTypeVariables #-}
module UserSpec (spec) where

import User
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "toString" $ do
    it "to string for no full user" $
        toString (NickUser "nick") `shouldBe` "no!full@user"