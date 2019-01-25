{-# LANGUAGE ScopedTypeVariables #-}
module ServerSpec (spec) where

import Server
import Client
import User
import TestHelper
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
