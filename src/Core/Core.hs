module Core.Core where

import Util.Util

import qualified Data.ByteString.Char8 as B
import System.Environment

type Command = [String] -> IO ()
type Hash = B.ByteString

data FileMode = StdMode | DirMode deriving (Eq)

instance Show FileMode where
  show StdMode = "100644"
  show DirMode = "40000"

instance Read FileMode where
  readsPrec _ = parsedModes . parseOctal
    where parsedModes 0o100644 = [(StdMode, "")]
          parsedModes 0o040000 = [(DirMode, "")]
          parsedModes _ = []

defaultEditor :: IO String
defaultEditor = getEnv "EDITOR"

authorName :: IO String
authorName = getEnv "OSHIT_AUTHOR"

authorEmail :: IO String
authorEmail = getEnv "OSHIT_EMAIL"
