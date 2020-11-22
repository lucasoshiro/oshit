module Commands.Plumbing.Object.ShowObj where

import Core.Object
import Core.Core

import qualified Data.ByteString.Char8 as B

showObj :: Command
showObj (hash:_) = do
  obj <- loadObject $ B.pack hash
  B.putStr $ objectContent obj
showObj _ = fail "hash missing"
