module Commands.Plumbing.Object.ShowObj where

import Core.Core
import Core.Object.Blob
import Core.Object.Object

import qualified Data.ByteString.Char8 as B

cmdShowObj :: Command
cmdShowObj (hash:_) = do
  obj <- loadObject $ B.pack hash :: IO Blob -- fix this.
  B.putStr $ objectRawContent obj
cmdShowObj _ = fail "hash missing"
