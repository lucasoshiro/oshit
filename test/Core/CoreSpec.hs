{-# LANGUAGE StandaloneDeriving #-}
module Core.CoreSpec (spec) where

import Test.Hspec

import Core.Core

deriving instance Eq   FileMode
deriving instance Show FileMode

spec :: Spec
spec = do
  describe "FileMode" $ do
    it "is properly converted to string" $ do
      stringFromFileMode StdMode `shouldBe` "100644"
      stringFromFileMode DirMode `shouldBe` "40000"

    it "is properly converted from string" $ do
      fileModeFromString "100644" `shouldBe` Just StdMode
      fileModeFromString "40000"  `shouldBe` Just DirMode
