module Commands.Plumbing.Index.TreeFromIndex where

import Core.Core
import Core.Index
import Core.Object

import qualified Data.ByteString.Char8 as B

cmdTreeFromIndex :: Command
cmdTreeFromIndex _ = do
  index <- readIndex
  let objs = treesFromIndex index
  let rootHash = hashObject $ head objs
  let objsIOs = map storeObject objs
  let outputHash = putStrLn $ B.unpack rootHash
  sequence_ $ objsIOs ++ [outputHash]

