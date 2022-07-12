module Commands.Plumbing.Merge.MergeCommit where

import Core.Core
import Core.Object
import Core.Merge

import Data.Time

import qualified Data.ByteString.Char8      as B

treeFromCommit :: Hash -> TreeIO
treeFromCommit hash = do
  commit <- loadCommit hash
  let (Commit {treeHash = treeHash}) = commit
  loadTree treeHash

cmdMergeCommit :: Command
cmdMergeCommit (o:a:b:author:email:_) = do
  let (hashO, hashA, hashB) = (B.pack o, B.pack a, B.pack b)
  let treeO = treeFromCommit hashO
  let treeA = treeFromCommit hashA
  let treeB = treeFromCommit hashB

  merged <- mergeTree treeO treeA treeB

  now <- getZonedTime
  msg <- getContents

  let commit = Commit (hashObject merged) [hashA, hashB] author email now msg
  storeObject commit
  putStrLn $ B.unpack . hashObject $ commit

cmdMergeCommit _ = fail "we need three commits for three way merging commits, and we need you name and email"
