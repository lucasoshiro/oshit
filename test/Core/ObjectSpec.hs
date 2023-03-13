module Core.ObjectSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Oshit (sha1Gen)

import Data.ByteString.Char8 (pack)
import Text.Printf (printf)
import System.Directory
import System.FilePath (takeDirectory)
import System.IO.Temp (createTempDirectory)

import Core.Object

spec :: Spec
spec = do
  describe "hashPath :: Hash -> FilePath" $ do
    prop "returns the path to the object in .git" $ forAll sha1Gen $ \hash -> do
      let expectedPath = printf ".git/objects/%s/%s" (take 2 hash) (drop 2 hash)
      hashPath (pack hash) `shouldBe` expectedPath

  describe "looseObjectExists :: String -> IO Bool" $ around_ withTempDir $ do
    prop "returns True if the object exists" $ forAll sha1Gen $ \hash -> do
      let objectPath = hashPath (pack hash)
      createDirectoryIfMissing True (takeDirectory objectPath)
      writeFile objectPath ""
      looseObjectExists hash `shouldReturn` True

    prop "returns False if the object does not exist" $ forAll sha1Gen $ \hash -> do
      looseObjectExists hash `shouldReturn` False

  describe "storeObject :: Object o => o -> IO ()" $ around_ withTempDir $ do
    prop "stores blobs in the .git directory" $ \blob -> do
      let blobPath = hashPath $ hashObject (blob :: Blob)
      storeObject blob
      doesFileExist blobPath `shouldReturn` True

withTempDir :: IO () -> IO ()
withTempDir action = do
  current <- getCurrentDirectory
  tempDir <- createTempDirectory current "hspec-util-utilspec"
  setCurrentDirectory tempDir -- work inside tmp directory
  action
  setCurrentDirectory current
  removeDirectoryRecursive tempDir
