module Util.UtilSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Oshit ()

import Data.Char (isSpace)
import Data.List (sort)
import Data.List.Index (indexed)
import Data.Map hiding (take, drop, map)
import Data.Text.Lazy (unpack)
import Formatting (format)
import Formatting.Formatters (oct)
import Safe (headMay, lastMay, initSafe)
import System.Directory
import System.IO.Temp (createTempDirectory)

import qualified Data.ByteString.Char8 as BS

import Util.Util

spec :: Spec
spec = do
  describe "parseOctal :: String -> Int" $ do
    prop "reliably parses an octal string" $ \i -> do
      parseOctal (unpack $ format oct (abs i)) `shouldBe` abs i

  describe "zipMap :: Ord k => Map k a -> Map k b -> Map k (a,b)" $ do
    prop "zips maps disregarding exclusive keys" $ \s -> do
      let mapA :: Map Int String
          mapA = fromList $ indexed s
          mapB = fromList . indexed $ drop 1 s
          mapC = zipMap mapA mapB
      shouldSatisfy (keys mapC) . all $ \k -> mapC ! k == (mapA ! k, mapB ! k)

  describe "trim :: String -> String" $ do
    prop "strips whitespace on the ends of a string" $ \s -> do
      headMay (trim s) `shouldSatisfy` maybe True (not . isSpace)
      lastMay (trim s) `shouldSatisfy` maybe True (not . isSpace)

    prop "doesn't alter strings with no edge whitespace" $ \s -> do
      trim ("(" ++ s ++ ")") `shouldBe` ("(" ++ s ++ ")")

  describe "sliceByteString :: [Int] -> ByteString -> [ByteString]" $ do
    prop "properly slices a sufficiently long bytestring" $ \spans' -> do
      let toBeSliced = BS.pack $ replicate (sum spans) ' '
          slices = sliceByteString spans toBeSliced
          spans = map abs spans'
      map BS.length slices `shouldBe` spans
      BS.concat slices `shouldBe` toBeSliced

    prop "adds empty slices for insufficiently long bytestring" $ \spans' -> do
      let toBeSliced = BS.pack $ replicate (sum $ initSafe spans) ' '
          mismatches = dropWhile (\(s,sl) -> BS.length sl == s) $ zip spans slices
          slices = sliceByteString spans toBeSliced
          spans = map abs spans'
      length slices `shouldSatisfy` flip elem [length spans', length spans' + 1]
      mismatches `shouldSatisfy` all (\(_,sl) -> sl == BS.empty)
      BS.concat slices `shouldBe` toBeSliced

    prop "returns a singleton or nothing when given no spans" $ \bs -> do
      sliceByteString [] bs `shouldBe` [bs | bs /= BS.empty]

  describe "invertMap :: Ord b => Map a b -> Map b [a]" $ do
    prop "properly inverts a random map" $ \m -> do
      let inverted = invertMap (m :: Map Int Int)
      shouldSatisfy inverted $ \inv -> all (\(a,b) -> a `elem` inv ! b) (toList m)

  describe "listDirectoryRecursive :: FilePath -> IO [FilePath]" $ do
    it "recursively lists directories" $ do
      current <- getCurrentDirectory
      tempDir <- createTempDirectory current "hspec-util-utilspec"

      setCurrentDirectory tempDir -- work inside tmp directory
      createDirectory "a" >> createDirectory "a/b"
      writeFile "a/b/c" "" >> writeFile "a/d" ""
      writeFile "e" ""

      files <- listDirectoryRecursive "."
      sort files `shouldBe` sort ["./a", "./a/b", "./a/b/c", "./a/d", "./e"]

      setCurrentDirectory current
      removeDirectoryRecursive tempDir
