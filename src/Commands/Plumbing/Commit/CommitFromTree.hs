module Commands.Plumbing.Commit.CommitFromTree where

import Core.Core
import Core.Object

import Data.Time

import qualified Data.ByteString.Char8 as B

cmdCommitFromTree :: Command
cmdCommitFromTree (au:em:tree:pars) = do
  now <- getZonedTime
  msg <- getContents
  let commit = Commit (B.pack tree) (map B.pack pars) au em now msg
  let hash = hashObject commit
  sequence_ [storeObject commit, putStrLn $ B.unpack hash]
cmdCommitFromTree _ = fail "not enough arguments"
