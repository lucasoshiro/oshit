module Commands.Porcelain.Log where

import Core.Core
import Core.Log
import Core.Object.Object
import Core.Reference
import Util.Colors
import Util.Util

import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map
import Data.List
import Data.Time

cmdLog :: Command
cmdLog (hash:_) = do
  hashCommits <- commitBFS $ B.pack hash

  branchMap <- do
    branches' <- allBranches :: IO [(Branch, Hash)]
    return . invertMap $ Map.fromList branches'

  let prettyCommit' = prettyCommit branchMap
    
  let prettyCommits = [prettyCommit' hash' commit | (hash', commit) <- hashCommits]

  putStrLn . intercalate "\n" $ prettyCommits

cmdLog [] = fail "commit not provided"
  
prettyCommit :: Map.Map Hash [Branch] -> Hash -> Commit -> String
prettyCommit branchMap hash (Commit _ _ author email timestamp message) =
  colorize yellow ("commit " ++ B.unpack hash) ++ " " ++ branchesStr ++ "\n" ++
  "Author: " ++ author ++ " <" ++ email ++ ">\n" ++
  "Date:   " ++ timestamp' ++ "\n" ++
  "\n" ++
  intercalate "\n" ["    " ++ line | line <- lines message] ++ "\n"
  where timestamp' = formatTime defaultTimeLocale logTimeFormat timestamp
        refBranches = if hash `Map.member` branchMap
                      then branchMap Map.! hash
                      else []
        branchesStr = "(" ++
                      (intercalate ", " $ map (colorize green) refBranches)
                      ++ ")"

logTimeFormat :: String
logTimeFormat = "%a %b %d %H:%M:%S %Y %Z"
