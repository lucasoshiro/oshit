module Core.CoreSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Oshit ()

import Control.Exception (evaluate)

import Core.Core

spec :: Spec
spec = do
  describe "FileMode" $ do
    prop "has an isomorphism in read/show" $ \fm -> do
      read (show fm) `shouldBe` (fm :: FileMode)

    context "when read from an invalid string" $ do
      it "throws an ErrorCall exception" $ do
        evaluate (read "123456" :: FileMode) `shouldThrow` anyErrorCall
