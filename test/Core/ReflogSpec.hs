module Core.ReflogSpec (spec) where

import Test.Hspec

import Data.ByteString.Char8 (unpack)

import Core.Reflog

exampleReflogEntryStr :: String
exampleReflogEntryStr =
  "9801739daae44ec5293d4e1f53d3f4d2d426d91c\
  \ 7ef021a2344b022be53f36a88758612924d94163\
  \ Example Author <author@email.com>\
  \ 1657669218 -0300\ttest: an example title"

spec :: Spec
spec = do
  describe "ReflogEntry" $ do
    let reflogEntry = parseReflogEntry exampleReflogEntryStr
        stringEntry = show reflogEntry

    it "is properly read from string" $ do
      unpack (oldHash reflogEntry) `shouldBe` "9801739daae44ec5293d4e1f53d3f4d2d426d91c"
      unpack (newHash reflogEntry) `shouldBe` "7ef021a2344b022be53f36a88758612924d94163"
      author reflogEntry `shouldBe` "Example Author"
      email reflogEntry `shouldBe` "author@email.com"
      title reflogEntry `shouldBe` "test: an example title"
      show (timestamp reflogEntry) `shouldBe` "2022-07-12 20:40:18 -0300"

    it "is properly converted to string" $ do
      stringEntry `shouldBe` exampleReflogEntryStr
