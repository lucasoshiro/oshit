module Commands.Porcelain.Merge.CherryPick where

import Core.Core
import Core.Object
import Core.Merge
import Core.Reference

import Data.Time

import qualified Data.ByteString.Char8      as B

cmdCherryPick :: Command
cmdCherryPick (hash:_) = do
  head' <- getHead

  branch <- case head' of
        (Left branch') -> return branch'
        (Right _)      -> fail "Cannot cherry-pick in detached HEAD"

  headCommitHash <- getHeadCommitHash
  headCommit <- loadCommit headCommitHash
  let Commit {treeHash = headTreeHash} = headCommit
  let headTree = loadTree headTreeHash

  cherryPickCommit <- loadCommit . B.pack $ hash
  let Commit { parents = cherryPickParents
             , treeHash = cherryPickTreeHash
             } = cherryPickCommit
  let cherryPickTree = loadTree cherryPickTreeHash

  parentCommitHash <-
    if cherryPickParents /= []
    then return . head $ cherryPickParents
    else fail "Cannot cherry-pick initial commit!"
  parentCommit <- loadCommit parentCommitHash
  let Commit {treeHash = parentTreeHash} = parentCommit
  let parentTree = loadTree parentTreeHash

  mergedTree <- mergeTree parentTree cherryPickTree headTree
  let mergedHash = hashObject mergedTree

  -- TODO: COMMITTER!!!!
  now <- getZonedTime
  let mergedCommit = cherryPickCommit { treeHash = mergedHash
                                      , parents = [headCommitHash]
                                      , timestamp = now
                                      }
  let mergeHash = hashObject mergedCommit

  storeObject mergedCommit
  updateBranch branch mergeHash
  putStrLn . B.unpack $ mergeHash

cmdCherryPick _ = fail "commit not provided"
