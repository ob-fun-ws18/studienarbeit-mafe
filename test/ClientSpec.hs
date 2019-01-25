{-# LANGUAGE ScopedTypeVariables #-}
module ClientSpec (spec) where

import Client
import User
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Client" $ do 
    describe "buildMsg" $ do
        it "builds a private message for a client" $
            buildMsg (FullUser "nick" "username" "full") (PrivMsg "receiver" "msg") `shouldBe` ":nick!username@localhost PRIVMSG receiver :msg"
        it "builds a join message" $
            buildMsg (FullUser "nick" "username" "full") (Join "tardis") `shouldBe` ":nick!username@localhost JOIN tardis"
        it "builds a part message" $
            buildMsg (FullUser "nick" "username" "full") (Part "tardis" "message") `shouldBe` ":nick!username@localhost PART tardis :message"
        it "builds a quit message" $
            buildMsg (FullUser "nick" "username" "full") (Quit "message") `shouldBe` ":nick!username@localhost QUIT :message"
        it "builds a is on message" $
            buildMsg (FullUser "nick" "username" "full") (IsOn ["a12", "b21", "c23"]) `shouldBe` ":localhost 303 nick :a12 b21 c23"            

    describe "parseMsg" $ do
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
        it "parses PART message without message" $
            parseMsg "PART #tardis" `shouldBe` Part "#tardis" ""
        it "parses PART message with message" $
            parseMsg "PART #tardis :bye" `shouldBe` Part "#tardis" "bye"
        it "parses PING message" $
            parseMsg "PING server" `shouldBe` Ping "server"
        it "parses QUIT message" $
            parseMsg "QUIT :bye" `shouldBe` Quit "bye"
        it "parses PLAIN message" $
            parseMsg "PLAIN alskd" `shouldBe` Plain "PLAIN alskd"