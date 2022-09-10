{-# LANGUAGE StandaloneDeriving #-}

module Test.Oshit where

-- -----------------------------------------------------------------------------
-- |
-- Module      : Test.Oshit
-- Description : Instances of the Arbitrary typeclass and some generators.
--
-- This module contains oprhan instances of the Test.QuickCheck.Arbitrary
-- typeclass as well as some useful generators for things such as author names,
-- emails, SHA1 hashes, etc.
--
-- Its purpose is to expose symbols to other test modules.
-- -----------------------------------------------------------------------------

import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

import qualified ASCII.Char as ASCII
import ASCII.Lists
import Data.ByteString.Char8 (ByteString, pack)
import Data.Char (chr)
import Data.Fixed (Pico)
import Data.Time.Lens (modL, seconds)
import Text.Printf (printf)

import Core.Core (FileMode(..))
import Core.Reflog (ReflogEntry(..))
import Core.Object (Blob(..))

deriving instance Show Blob

-- | Orphan instance of MonadFail for Test.QuickCheck.Gen, for pattern matching.
instance MonadFail Gen where
  fail = error

-- | Generate a random String from a provided subset of the ASCII table.
asciiGen :: [ASCII.Char] -> Int -> Gen String
asciiGen s n = vectorOf n . elements $ map (chr . ASCII.toInt) s

-- | Generate a random SHA1 hash.
sha1Gen :: Gen String
sha1Gen = asciiGen hexDigits 40

-- | Generate a random author name with up to 4 words.
authorGen :: Gen String
authorGen = chooseInt (2, 4) >>= \names ->
  fmap unwords . vectorOf names . sized $ asciiGen letters . (+ 1)

-- | Generate a random email address.
emailGen :: Gen String
emailGen = do
  let smallNameGen = sized $ asciiGen smallLetters . (+ 1)
      emailFieldsGen = vectorOf 3 smallNameGen
  [u, d, t] <- emailFieldsGen
  return $ printf "%s@%s.%s" u d t

-- | Mathematical floor that applies to a Pico value. Here to force Int range
-- constraints and avoid the type-defaults warning.
picoFloor :: Pico -> Pico
picoFloor = (fromIntegral :: Int -> Pico) . floor

-- | Generate a random ReflogEntry by instantiating Arbitrary. Due to Git's time
-- format, the randomly generated timestamp has its seconds floor'd.
instance Arbitrary ReflogEntry where
  arbitrary = do
    [oldH, newH] <- vectorOf 2 $ fmap pack sha1Gen
    au <- authorGen
    em <- emailGen
    st <- modL seconds picoFloor <$> arbitrary
    tt <- sized $ asciiGen printableCharacters . (+ 1)
    return ReflogEntry { oldHash = oldH, newHash = newH
                       , author = au, email = em
                       , timestamp = st, title = tt
                       }

-- | Generate a random FileMode.
instance Arbitrary FileMode where
  arbitrary = elements [ StdMode, DirMode ]

-- | Generate a random ByteString from a random String.
instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

-- | Generate a random Blob.
instance Arbitrary Blob where
  arbitrary = Blob <$> arbitrary
