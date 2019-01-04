{-# LANGUAGE ScopedTypeVariables #-}
module ClientSpec (spec) where

import Client
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "parseMsg" $ do
    it "parses NICK message" $
        parseMsg "NICK amy" `shouldBe` Nick "amy"