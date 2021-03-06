module Core.Core where

import qualified Data.ByteString.Char8 as B
import System.Environment

type Command = [String] -> IO ()
type FileMode = String
type Hash = B.ByteString

defaultEditor :: IO String
defaultEditor = getEnv "EDITOR"

authorName :: IO String
authorName = getEnv "OSHIT_AUTHOR"

authorEmail :: IO String
authorEmail = getEnv "OSHIT_EMAIL"

standardMode :: FileMode
standardMode = "100644"

dirMode :: FileMode
dirMode = "040000"
