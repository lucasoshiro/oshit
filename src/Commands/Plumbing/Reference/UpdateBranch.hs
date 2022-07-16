module Commands.Plumbing.Reference.UpdateBranch where

import qualified Data.ByteString.Char8      as B
import qualified System.Directory           as Dir

import Core.Core
import Core.Object
import Core.Reference

cmdUpdateBranch :: Command
cmdUpdateBranch (branch:hashStr:_) = do
  let hash = B.pack hashStr
  -- TODO: packfile
  hashExists <- looseObjectExists hashStr
  if hashExists
    then updateBranch branch hash
    else fail "invalid hash"
cmdUpdateBranch (_:[]) = fail "commit not provided"
cmdUpdateBranch [] = fail "branch not provided"
