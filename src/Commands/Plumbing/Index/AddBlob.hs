module Commands.Plumbing.Index.AddBlob where

import Core.Core
import Core.Index

import qualified Data.ByteString.Char8 as B

cmdAddBlob :: Command
cmdAddBlob (hsh:path:_) = do
  old <- readIndex
  let new = addBlobToIndex path (B.pack hsh) old
  storeIndex new
cmdAddBlob (_:_) = fail "path not provided"
cmdAddBlob _ = fail "hash not provided"
