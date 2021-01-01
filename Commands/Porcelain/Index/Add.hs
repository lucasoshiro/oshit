module Commands.Porcelain.Index.Add where

import Core.Core
import Core.Index

cmdAdd :: Command
cmdAdd [] = fail "File not provided."
cmdAdd files = do
  index <- readIndex
  updateAndStoreIndex index files
