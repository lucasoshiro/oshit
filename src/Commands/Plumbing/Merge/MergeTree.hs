module Commands.Plumbing.Merge.MergeTree where

import Core.Core
import Core.Object
import Core.Merge

import qualified Data.ByteString.Char8      as B

cmdMergeTree :: Command
cmdMergeTree (o:a:b:_) = do
  let (treeO, treeA, treeB) = (hashToTree o, hashToTree a, hashToTree b)
      hashToTree = loadObject . B.pack

  merged <- mergeTree treeO treeA treeB
  putStrLn $ B.unpack . hashObject $ merged

cmdMergeTree _ = fail "we need three trees for three way merging trees..."
