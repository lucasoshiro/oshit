module Commands.Plumbing.Reference.UpdateHead where

import qualified Data.ByteString.Char8      as B

import Core.Core
import Core.Object.Object
import Core.Reference

cmdUpdateHead :: Command
cmdUpdateHead (ref:_) = do
  hashExists <- objectExists ref
  let hash = B.pack ref
  if hashExists
    then updateHead $ Right hash
    else updateHead $ Left ref
cmdUpdateHead _ = fail "reference not provided"
  
