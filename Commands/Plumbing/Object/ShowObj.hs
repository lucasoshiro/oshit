module Commands.Plumbing.Object.ShowObj where

import Core.Object
import Core.Core

import qualified Data.ByteString.Char8 as B

cmdShowObj :: Command
cmdShowObj (hash:_) = do
  obj <- loadObject $ B.pack hash
  B.putStr $ objectContent obj
cmdShowObj _ = fail "hash missing"
