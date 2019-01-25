{-# LANGUAGE ScopedTypeVariables #-}
module ServerSpec (spec) where

import Server
import Client
import User
import TestHelper
import qualified Data.Map as Map
import Data.Map((!))
import System.IO
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Server" $ do
    describe "isFinalMsg" $ do
        it "detects Quit correctly" $
            isFinalMsg (Quit "foobar") `shouldBe` True
        it "detects non-quit correctly" $
            isFinalMsg (Join "#foo") `shouldBe` False

    describe "handleMsg" $ do
        it "sets nickname for NoUser" $ withHandle $ \h ->
            handleMsg h NoUser (Nick "foo") `shouldBe` (NickUser "foo", Nothing)
        it "sets information for NickUser and generates event" $ withHandle $ \h ->
            handleMsg h (NickUser "foo") (User "bar" "name") `shouldBe` (FullUser "foo" "bar" "name",Just (NewUser (FullUser "foo" "bar" "name") h))
        it "ignores other message and generates event" $ withHandle $ \h ->
            handleMsg h (NickUser "foo") (Join "#foo") `shouldBe` (NickUser "foo",Just (ClientMsg (NickUser "foo") (Join "#foo")))

    describe "handleEvent" $ do
        it "handles a new user" $ withHandle $ \h -> do
            let users = Map.empty
                channels = Map.empty
            pos <- hGetPosn h
            (new_users, new_channels) <- handleEvent users channels (NewUser (FullUser "nick" "user" "name") h)
            hSetPosn pos
            reply <- hGetLine h
            reply `shouldBe` ":localhost 001 nick :Welcome to the Internet Relay Network nick!user@localhost"
            Map.size new_users `shouldBe` 1
            new_channels `shouldBe` channels
        it "handles a Pong event" $ withHandle $ \h -> do
            let users = Map.insert "nick" h Map.empty
                channels = Map.empty
                msg = ClientMsg (FullUser "nick" "user" "name") (Ping "server")
            pos <- hGetPosn h
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos
            reply <- hGetLine h
            reply `shouldBe` "PONG localhost"
            new_users `shouldBe` users
            new_channels `shouldBe` channels
        it "handles private message to user" $ withHandle $ \h -> do
            let users = Map.insert "receiver" h Map.empty
                channels = Map.empty
                msg = ClientMsg (FullUser "nick" "user" "name") (PrivMsg "receiver" "msg")
            pos <- hGetPosn h
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos
            reply <- hGetLine h
            reply `shouldBe` ":nick!user@localhost PRIVMSG receiver :msg"
            new_users `shouldBe` users
            new_channels `shouldBe` channels
        it "handles private message to channel" $ withHandle $ \h -> do
            let users = Map.insert "receiver" h Map.empty
                channels = Map.insert "#channel" ["receiver"] Map.empty
                msg = ClientMsg (FullUser "nick" "user" "name") (PrivMsg "#channel" "msg")
            pos <- hGetPosn h
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos
            reply <- hGetLine h
            reply `shouldBe` ":nick!user@localhost PRIVMSG #channel :msg"
            new_users `shouldBe` users
            new_channels `shouldBe` channels
        it "handels a JOIN a channel message" $ withHandle $ \h1 -> withHandle $ \h2 -> do
            let users = Map.insert "user1" h1 (Map.insert "user2" h2 Map.empty)
                channels = Map.insert "#channel" ["user1"] Map.empty
                msg = ClientMsg (FullUser "user2" "user" "name") (Join "#channel")
            pos1 <- hGetPosn h1
            pos2 <- hGetPosn h2
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos1
            hSetPosn pos2
            reply1 <- hGetLine h1
            reply2 <- hGetLine h2
            reply1 `shouldBe` ":user2!user@localhost JOIN #channel"
            reply2 `shouldBe` ":user2!user@localhost JOIN #channel"
            new_users `shouldBe` users
            length (new_channels ! "#channel") `shouldBe` 2
        it "handels a JOIN multi channels message" $ withHandle $ \h1 -> withHandle $ \h2 -> do
            let users = Map.insert "user1" h1 (Map.insert "user2" h2 Map.empty)
                channels = Map.insert "#channel" ["user1"] (Map.insert "channel2" ["user1"] Map.empty)
                msg = ClientMsg (FullUser "user2" "user" "name") (JoinMulti ["#channel", "#channel2"])
            pos1 <- hGetPosn h1
            pos2 <- hGetPosn h2
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos1
            hSetPosn pos2
            reply1 <- hGetLine h1
            reply2 <- hGetLine h2
            reply1 `shouldBe` ":user2!user@localhost JOIN #channel"
            reply2 `shouldBe` ":user2!user@localhost JOIN #channel"
            new_users `shouldBe` users
            length (new_channels ! "#channel") `shouldBe` 2
        it "handels a PART message" $ withHandle $ \h1 -> withHandle $ \h2 -> do
            let users = Map.insert "user1" h1 (Map.insert "user2" h2 Map.empty)
                channels = Map.insert "#channel" ["user1", "user2"] Map.empty
                msg = ClientMsg (FullUser "user2" "user" "name") (Part "#channel" "bye")
            pos1 <- hGetPosn h1
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos1
            reply1 <- hGetLine h1
            reply1 `shouldBe` ":user2!user@localhost PART #channel :bye"
            new_users `shouldBe` users
            (new_channels ! "#channel") `shouldBe` ["user1"]
        it "handels a QUIT message" $ withHandle $ \h1 -> withHandle $ \h2 -> do
            let users = Map.insert "user1" h1 (Map.insert "user2" h2 Map.empty)
                channels = Map.insert "#channel" ["user1", "user2"] Map.empty
                msg = ClientMsg (FullUser "user2" "user" "name") (Quit "bye")
            pos1 <- hGetPosn h1
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos1
            reply1 <- hGetLine h1
            reply1 `shouldBe` ":user2!user@localhost QUIT :bye"
            Map.size new_users `shouldBe` 1
            length (new_channels ! "#channel") `shouldBe` 1
        it "handels a IsOn message" $ withHandle $ \h1 -> withHandle $ \h2 -> do
            let users = Map.insert "user1" h1 (Map.insert "user2" h2 Map.empty)
                channels = Map.empty
                msg = ClientMsg (FullUser "user2" "user" "name") (IsOn ["user1"])
            pos2 <- hGetPosn h2
            (new_users, new_channels) <- handleEvent users channels msg
            hSetPosn pos2
            reply2 <- hGetLine h2
            reply2 `shouldBe` ":localhost 303 user2 :user1"
            new_users `shouldBe` users
            new_channels `shouldBe` channels
