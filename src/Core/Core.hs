module Core.Core where

import Util.Util

import qualified Data.ByteString.Char8 as B
import System.Environment

type Command = [String] -> IO ()
type Hash = B.ByteString

data FileMode = StdMode | DirMode

defaultEditor :: IO String
defaultEditor = getEnv "EDITOR"

authorName :: IO String
authorName = getEnv "OSHIT_AUTHOR"

authorEmail :: IO String
authorEmail = getEnv "OSHIT_EMAIL"

fileModeFromString :: String -> Maybe FileMode
fileModeFromString = fileModeFromString' . parseOctal
  where fileModeFromString' :: Int -> Maybe FileMode
        fileModeFromString' 0o100644 = Just StdMode
        fileModeFromString' 0o040000 = Just DirMode
        fileModeFromString' _        = Nothing

stringFromFileMode :: FileMode -> String
stringFromFileMode StdMode = "100644"
stringFromFileMode DirMode = "40000"
