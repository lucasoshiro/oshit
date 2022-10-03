module Core.ReflogSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Oshit ()

import Data.Time.LocalTime (zonedTimeToUTC)

import Core.Reflog

spec :: Spec
spec = do
  describe "ReflogEntry" $ do
    prop "has an isomorphism in read/show" $ \entry -> do
      let readEntry = read $ show entry
          timestamp' = zonedTimeToUTC . timestamp
      oldHash readEntry    `shouldBe` oldHash entry
      newHash readEntry    `shouldBe` newHash entry
      author readEntry     `shouldBe` author entry
      email readEntry      `shouldBe` email entry
      title readEntry      `shouldBe` title entry
      timestamp' readEntry `shouldBe` timestamp' entry
      show readEntry       `shouldBe` show entry
