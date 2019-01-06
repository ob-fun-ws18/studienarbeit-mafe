{-# LANGUAGE ScopedTypeVariables #-}
module ClientSpec (spec) where

import Client
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "parseMsg" $ do
    it "parses NICK message" $
        parseMsg "NICK amy" `shouldBe` Nick "amy"
    it "parses USER message" $
        parseMsg "USER user foo bar :User Name" `shouldBe` User "user" "User Name"
    it "parses minimal USER message" $
        parseMsg "USER user 0 * :Name" `shouldBe` User "user" "Name"
    it "parses PRIVMSG to user" $
        parseMsg "PRIVMSG rory :Hello..." `shouldBe` PrivMsg "rory" "Hello..."
    it "parses JOIN message" $
        parseMsg "JOIN #tardis" `shouldBe` JoinMulti ["#tardis"]
    it "parses JOIN message with multiple channels" $
        parseMsg "JOIN #tardis,#abc,#def" `shouldBe` JoinMulti ["#tardis", "#abc", "#def"]
    it "parses PART message" $
        parseMsg "PART #tardis :Off to save Rory" `shouldBe` Part "#tardis" "Off to save Rory"
    it "parses ISON message with multiple users" $
        parseMsg "ISON abc def" `shouldBe` IsOn ["abc", "def"]
    it "parses ISON message with one user" $
        parseMsg "ISON abc" `shouldBe` IsOn ["abc"]