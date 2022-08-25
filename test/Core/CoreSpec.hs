module Core.CoreSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Oshit ()

import Core.Core

spec :: Spec
spec = do
  describe "FileMode" $ do
    prop "has an isomorphism in read/show" $ \fm -> do
      read (show fm) `shouldBe` (fm :: FileMode)
