module Commands.Porcelain.Log where

import Core.Core
import Core.Log
import Core.Object.Object
import Util.Colors

import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Time

cmdLog :: Command
cmdLog (hash:_) = do
  hashCommits <- commitBFS $ B.pack hash
  let prettyCommits = [prettyCommit hash' commit | (hash', commit) <- hashCommits]
  putStrLn . intercalate "\n" $ prettyCommits
cmdLog [] = fail "commit not provided"
  
prettyCommit :: Hash -> Commit -> String
prettyCommit hash (Commit _ _ author email timestamp message) =
  colorize yellow ("commit " ++ B.unpack hash) ++ "\n" ++
  "Author: " ++ author ++ " <" ++ email ++ ">\n" ++
  "Date:   " ++ timestamp' ++ "\n" ++
  "\n" ++
  intercalate "\n" ["    " ++ line | line <- lines message] ++ "\n"
  where timestamp' = formatTime defaultTimeLocale logTimeFormat timestamp

logTimeFormat :: String
logTimeFormat = "%a %b %d %H:%M:%S %Y %Z"
